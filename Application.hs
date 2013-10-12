{-# LANGUAGE CPP #-}
module Application where

import Prelude (show)
import BasicPrelude hiding (show)

import Numeric (showHex)
import Control.Error (note, throwT, scriptIO, eitherT, EitherT(..))
import System.Random (randomR, getStdRandom)

import Network.Wai (Response(..), Application)
import Network.Wai.Util
import Network.Mail.Mime (Address(..), Mail(..), renderSendMail)
import Network.HTTP.Types (ok200, notFound404, seeOther303, badRequest400)

import Network.Wai.Digestive (bodyFormEnv_)
import SimpleForm.Digestive.Combined (SimpleForm', postSimpleForm, input_)
import SimpleForm.Combined (SelectEnum(..), unSelectEnum)
import SimpleForm.Render.XHTML5 (render)

import Text.Email.Validate (EmailAddress)
import qualified Data.Text as T
import Network.URI (URI)
import qualified Network.URI.Partial as PartialURI

import MustacheTemplates
import Database
import Records
#include "PathHelpers.hs"

s :: (IsString s) => String -> s
s = fromString

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
errorPage msg = textBuilder badRequest400 headers (viewError htmlEscape $ ErrorMessage msg)
	where
	Just headers = stringHeaders [("Content-Type", "text/html; charset=utf8")]

on404 :: Application
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

home :: URI -> DatabaseConnection -> Application
home root db _ = liftIO uniqId >>=
	redirect' seeOther303 [] . (`PartialURI.relativeTo` root) . showGamePath
	where
	uniqId = do
		id <- (`showHex` "") <$> getStdRandom (randomR (minBound,maxBound :: Word32))
		v <- dbGet db id
		case v of
			Nothing -> return id
			_ -> uniqId

showGame :: URI -> DatabaseConnection -> String -> Application
showGame _ db id req = do
	context <- ctx <$> dbGet db id
	handleAcceptTypes [
		("text/html",
			textBuilder ok200 htmlHeader (viewRps htmlEscape context)),
		("application/json", json ok200 [] context)
		] req
	where
	Just htmlHeader = stringHeaders [("Content-Type", "text/html; charset=utf8")]

createChoiceForm :: (Monad m) => SimpleForm' m (EmailAddress, RPSChoice)
createChoiceForm = do
	email' <- input_ (s"email") (Just . fst)
	choice' <- input_ (s"choice") (Just . SelectEnum . snd)
	return $ (,) <$> email' <*> fmap unSelectEnum choice'

createChoice :: URI -> DatabaseConnection -> String -> Application
createChoice root db gid req = eitherT errorPage return $ do
	(email, choice) <- EitherT $ fmap (note "Invalid selection" . snd) $ postSimpleForm render (bodyFormEnv_ req) createChoiceForm
	v <- scriptIO $ dbGet db gid
	case v of
		Nothing -> scriptIO $ dbSet db gid (RPSGameStart (email,choice))
		Just (RPSGameStart a) -> do
			let rpsGame = RPSGameFinish a (email, choice)
			let context = ctx (Just rpsGame)
			let winTxt = case winner context of
				Just e -> show e ++ " wins!"
				Nothing -> "It's a tie!"
			scriptIO $ dbSet db gid rpsGame
			mailBody <- responseToMailPart True =<< textBuilder ok200 [] (viewEmail id context)
			scriptIO $ renderSendMail Mail {
					mailFrom    = appEmail,
					mailTo      = [emailToAddress (fst a), emailToAddress email],
					mailCc      = [], mailBcc  = [],
					mailHeaders = [(fromString "Subject", T.pack $ "[Roshambo "++gid++"] " ++ winTxt)],
					mailParts   = [[mailBody]]
				}
		_ -> throwT "You cannot make a new choice on a completed game!"
	redirect' seeOther303 [] (showGamePath gid `PartialURI.relativeTo` root)
	where
	emailToAddress = Address Nothing . T.pack . show
