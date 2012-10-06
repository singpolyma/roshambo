{-# LANGUAGE CPP #-}
module Application where

import Prelude (show)
import BasicPrelude hiding (intercalate, show)

import Numeric (showHex)
import Data.String (IsString, fromString)
import Control.Error (hush, note, fmapL, readErr)
import System.Random (randomR, getStdRandom)
import System.IO.Error (ioeGetErrorString)

import Network.Wai (Request(..), Response(..))
import Network.Wai.Parse (parseRequestBody, parseHttpAccept)
import Network.Wai.Util
import Network.HTTP.Accept (selectAcceptType)
import Network.Mail.Mime (Address(..), Mail(..), renderSendMail)
import Network.HTTP.Types (ok200, notFound404, seeOther303, badRequest400, notAcceptable406)
import Control.Monad.Trans.Resource (transResourceT, ResourceT)

import Text.Email.Validate (EmailAddress)
import qualified Text.Email.Validate as EmailAddress (validate)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.URI as URI

import EitherIO
import MustacheTemplates
import Database
import Records
#include "PathHelpers.hs"

type ApplicationEitherIO = Request -> ResourceT EitherIO Response

htmlEscape :: String -> String
htmlEscape = concatMap escChar
	where
	escChar '&' = "&amp;"
	escChar '"' = "&quot;"
	escChar '<' = "&lt;"
	escChar '>' = "&gt;"
	escChar c   = [c]

appEmail :: Address
appEmail = Address (Just $ T.pack "Roshambo App") (T.pack "roshambo@example.com")

errorPage :: (MonadIO m, Functor m) => String -> m Response
errorPage msg = return $ ResponseBuilder badRequest400 headers (viewError htmlEscape $ ErrorMessage msg)
	where
	Just headers = stringHeaders [("Content-Type", "text/html; charset=utf8")]

on404 :: ApplicationEitherIO
on404 _ = string notFound404 [] "Not Found"

rpsWinner :: (EmailAddress, RPSChoice) -> (EmailAddress, RPSChoice) -> Maybe EmailAddress
rpsWinner (e,Paper)    (_,Rock)     = Just e
rpsWinner (_,Rock)     (e,Paper)    = Just e
rpsWinner (e,Rock)     (_,Scissors) = Just e
rpsWinner (_,Scissors) (e,Rock)     = Just e
rpsWinner (e,Scissors) (_,Paper)    = Just e
rpsWinner (_,Paper)    (e,Scissors) = Just e
rpsWinner _ _ = Nothing

ctxChoice :: (EmailAddress,RPSChoice) -> ChoiceRecord
ctxChoice (e,c) = ChoiceRecord (show e) (show c)

ctx :: Maybe RPSGame -> GameContext
ctx (Just (RPSGameStart a)) = GameContext Nothing False [ctxChoice a]
ctx (Just (RPSGameFinish a b)) =
	GameContext (map show $ rpsWinner a b) True [ctxChoice a, ctxChoice b]
ctx _ = GameContext Nothing False []

home :: URI -> DatabaseConnection -> ApplicationEitherIO
home root db _ = liftIO uniqId >>=
	redirect' seeOther303 [] . URI.relativeTo root . showGamePath
	where
	uniqId = do
		id <- (`showHex` "") <$> getStdRandom (randomR (minBound,maxBound :: Word32))
		v <- dbGet db id
		case v of
			Nothing -> return id
			_ -> uniqId

showGame :: URI -> DatabaseConnection -> String -> ApplicationEitherIO
showGame _ db id req = do
	context <- ctx <$> dbGet db id
	case acceptType of
		"text/html" ->
			return $ ResponseBuilder ok200 htmlHeader (viewRps htmlEscape context)
		"application/json" ->
			json ok200 [] context
		_ -> string notAcceptable406 [] (intercalate "\n" supportedTypes)
	where
	Just htmlHeader = stringHeaders [("Content-Type", "text/html; charset=utf8")]
	acceptType = fromMaybe (head supportedTypes) acceptType'
	acceptType' = (selectAcceptType supportedTypes . parseHttpAccept) =<<
		lookup (fromString "Accept") (requestHeaders req)
	supportedTypes = ["text/html", "application/json"]

requiredParam :: (Eq k) => e -> (v -> Maybe v) -> (v -> Either e a) -> k -> [(k, v)] -> Either e a
requiredParam notPresent maybePresent parser k =
	parser <=< note notPresent . (maybePresent <=< lookup k)

optionalParam :: (Eq k) => (v -> Maybe v) -> (v -> Either e a) -> k -> [(k, v)] -> Maybe a
optionalParam maybePresent parser k =
	hush . parser <=< maybePresent <=< lookup k

blankNotPresent :: Text -> Maybe Text
blankNotPresent t
	| T.null t  = Nothing
	| otherwise = Just t

parseCreateChoiceRequest :: [(Text, Text)] -> Either String (EmailAddress, RPSChoice)
parseCreateChoiceRequest body = (,)
	<$> requiredParam "You didn't send an email address!" blankNotPresent parseEmailErr (T.pack "email") body
	<*> requiredParam "You didn't send a choice!" blankNotPresent readRPSErr (T.pack "choice") body
	where
	readRPSErr choiceText = let c = T.unpack choiceText in
		readErr ("\""++c++"\" is not one of: Rock, Paper, Scissors") c
	parseEmailErr emailText = let e = T.unpack emailText in
		fmapL (\err -> "Error parsing email <" ++ e ++ ">\n" ++ show err)
			(EmailAddress.validate e)

createChoice :: URI -> DatabaseConnection -> String -> ApplicationEitherIO
createChoice root db gid req = transResourceT (eitherIO (errorPage.ioeGetErrorString) return) $ do
	(body, _) <- transResourceT liftIO $ first (map (T.decodeUtf8 *** T.decodeUtf8)) <$> parseRequestBody noStoreFileUploads req
	(email, choice) <- lift $ hoistIO $ return $ fmapL userError $ parseCreateChoiceRequest body
	v <- liftIO $ dbGet db gid
	case v of
		Nothing -> liftIO $ dbSet db gid (RPSGameStart (email,choice))
		Just (RPSGameStart a) -> do
			let rpsGame = RPSGameFinish a (email, choice)
			let context = ctx (Just rpsGame)
			let winTxt = case winner context of
				Just e -> show e ++ " wins!"
				Nothing -> "It's a tie!"
			liftIO $ dbSet db gid rpsGame
			mailBody <- responseToMailPart True $ ResponseBuilder ok200 [] (viewEmail id context)
			liftIO $ renderSendMail Mail {
					mailFrom    = appEmail,
					mailTo      = [emailToAddress (fst a), emailToAddress email],
					mailCc      = [], mailBcc  = [],
					mailHeaders = [(fromString "Subject", T.pack $ "[Roshambo "++gid++"] " ++ winTxt)],
					mailParts   = [[mailBody]]
				}
		_ -> failIO "You cannot make a new choice on a completed game!"
	redirect' seeOther303 [] (URI.relativeTo root $ showGamePath gid)
	where
	emailToAddress = Address Nothing . T.pack . show
