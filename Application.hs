{-# LANGUAGE CPP #-}
module Application where

import Prelude (show)
import BasicPrelude hiding (intercalate, show)

import Data.Maybe (listToMaybe)
import Data.Char (isAscii)
import Numeric (showHex)
import Data.String (IsString, fromString)
import Control.Error (eitherT, throwT, hush, note, hoistEither, fmapL, readErr, EitherT(..), scriptIO)
import System.Random (randomR, getStdRandom)

import Network.Wai (Request(..), Response(..), responseLBS, responseSource)
import Network.Wai.Parse (parseRequestBody, BackEnd, parseHttpAccept)
import Network.HTTP.Types (ok200, notFound404, seeOther303, badRequest400, notAcceptable406, statusIsRedirection, Status(..), ResponseHeaders, Header)
import Data.Conduit (($$), runResourceT, Flush(..), ResourceT)
import Data.Conduit.List (fold, sinkNull)

import qualified Data.ByteString as BS (split)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Text.Email.Validate (EmailAddress)
import qualified Text.Email.Validate as EmailAddress (validate)
import Network.URI (URI(..), uriIsAbsolute)
import qualified Network.URI as URI
import Network.Mail.Mime
import qualified Data.Aeson as Aeson
import qualified Blaze.ByteString.Builder as Builder
import qualified Blaze.ByteString.Builder.Char.Utf8 as Builder
import qualified Data.CaseInsensitive as CI

import MustacheTemplates
import Database
import Records
#include "PathHelpers.hs"

htmlEscape :: String -> String
htmlEscape = concatMap escChar
	where
	escChar '&' = "&amp;"
	escChar '"' = "&quot;"
	escChar '<' = "&lt;"
	escChar '>' = "&gt;"
	escChar c   = [c]

uriRelativeTo :: URI -> URI -> URI
uriRelativeTo other root = let Just uri = URI.relativeTo root other in uri

noStoreFileUploads :: BackEnd ()
noStoreFileUploads _ _ = sinkNull

responseToMailPart :: (MonadIO m) => Bool -> Response -> m Part
responseToMailPart asTxt r = do
	body <- liftIO $ Builder.toLazyByteString `fmap` builderBody
	return $ Part (T.decodeUtf8 contentType) contentEncode Nothing headers body
	where
	chunkFlatAppend m (Chunk more) = m `mappend` more
	chunkFlatAppend m _ = m
	headers = map (CI.original *** T.decodeUtf8) $ filter ((/=contentTypeName) . fst) headers'
	contentType = fromMaybe defContentType $ lookup contentTypeName headers'
	contentEncode  | asTxt     = QuotedPrintableText
	               | otherwise = Base64
	defContentType | asTxt     = fromString "text/plain; charset=utf-8"
	               | otherwise = fromString "application/octet-stream"
	builderBody = runResourceT $ body' $$ fold chunkFlatAppend mempty
	(_, headers', body') = responseSource r
	contentTypeName = fromString "Content-Type"

mapHeader :: (ResponseHeaders -> ResponseHeaders) -> Response -> Response
mapHeader f (ResponseFile s h b1 b2) = ResponseFile s (f h) b1 b2
mapHeader f (ResponseBuilder s h b) = ResponseBuilder s (f h) b
mapHeader f (ResponseSource s h b) = ResponseSource s (f h) b

defHeader :: Header -> Response -> Response
defHeader h = mapHeader (defHeader' h)

defHeader' :: Header -> ResponseHeaders -> ResponseHeaders
defHeader' (n, v) headers = case lookup n headers of
		Just _  -> headers
		Nothing -> (n, v):headers

replaceHeader :: Header -> Response -> Response
replaceHeader h = mapHeader (replaceHeader' h)

replaceHeader' :: Header -> ResponseHeaders -> ResponseHeaders
replaceHeader' (n, v) = ((n,v):) . filter ((/=n) . fst)

string :: (MonadIO m) => Status -> ResponseHeaders -> String -> m Response
string status headers = return . defHeader defCT . ResponseBuilder status headers . Builder.fromString
	where
	Just defCT = stringHeader ("Content-Type", "text/plain; charset=utf-8")

text :: (MonadIO m) => Status -> ResponseHeaders -> Text -> m Response
text status headers = return . defHeader defCT . ResponseBuilder status headers . Builder.fromText
	where
	Just defCT = stringHeader ("Content-Type", "text/plain; charset=utf-8")

json :: (MonadIO m, Aeson.ToJSON a) => Status -> ResponseHeaders -> a -> m Response
json status headers = return . defHeader defCT . responseLBS status headers . Aeson.encode . Aeson.toJSON
	where
	Just defCT = stringHeader ("Content-Type", "application/json; charset=utf-8")

redirect :: Status -> ResponseHeaders -> URI -> Maybe Response
redirect status headers uri
	| statusIsRedirection status && uriIsAbsolute uri = do
		uriBS <- stringAscii (show uri)
		return $ responseLBS status ((location, uriBS):headers) mempty
	| otherwise = Nothing
	where
	Just location = stringAscii "Location"

redirect' :: (Monad m) => Status -> ResponseHeaders -> URI -> m Response
redirect' status headers uri =
	let Just r = redirect status headers uri in return r

stringAscii :: (IsString s) => String -> Maybe s
stringAscii s
	| all isAscii s = Just (fromString s)
	| otherwise     = Nothing

stringHeader :: (IsString s1, IsString s2) => (String, String) -> Maybe (s1, s2)
stringHeader (n, v) = liftM2 (,) (stringAscii n) (stringAscii v)

stringHeaders :: (IsString s1, IsString s2) => [(String, String)] -> Maybe [(s1, s2)]
stringHeaders = mapM stringHeader

stringHeaders' :: (IsString s1, IsString s2) => [(String, String)] -> [(s1, s2)]
stringHeaders' hs = let Just headers = stringHeaders hs in headers

data Pattern a = PatternAny | PatternExactly a

instance (Eq a) => Eq (Pattern a) where
	PatternAny == _ = True
	_ == PatternAny = True
	(PatternExactly a) == (PatternExactly b) = a == b

selectAcceptType :: [String] -> [ByteString] -> Maybe String
selectAcceptType supported accept = case supported' of
	Just sup -> listToMaybe $ mapMaybe (`lookup` sup) accept'
	Nothing -> Nothing
	where
	accept' = map parseAccept accept
	supported' = fmap (`zip` supported)
		(mapM (fmap parseAccept . stringAscii) supported)
	parsePattern s | s == fromString "*" = PatternAny
	               | otherwise = PatternExactly s
	parseAccept s = let (t:sub:_) = BS.split 0x2f s in
		(parsePattern t, parsePattern sub)

bodyBytestring :: Request -> ResourceT IO ByteString
bodyBytestring req = requestBody req $$ fold (++) empty

on404 _ = string notFound404 [] "Not Found"

appEmail :: Address
appEmail = Address (Just $ T.pack "Roshambo App") (T.pack "roshambo@example.com")

errorPage :: (MonadIO m, Functor m) => String -> m Response
errorPage msg = return $ ResponseBuilder badRequest400 headers (viewError htmlEscape $ ErrorMessage msg)
	where
	Just headers = stringHeaders [("Content-Type", "text/html; charset=utf8")]

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

home root db _ = do
	id <- uniqId
	redirect' seeOther303 [] (uriRelativeTo root $ showGamePath id)
	where
	-- Can't liftIO the do block directly because of the loop
	uniqId = liftIO uniqId'
	uniqId' = do
		id <- (`showHex` "") <$> getStdRandom (randomR (minBound,maxBound :: Word32))
		v <- dbGet db id
		case v of
			Nothing -> return id
			_ -> uniqId'

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
	<*> requiredParam "You didn't send a choice!" Just readRPSErr (T.pack "choice") body
	where
	readRPSErr choiceText = let c = T.unpack choiceText in
		readErr ("\""++c++"\" is not one of: Rock, Paper, Scissors") c
	parseEmailErr emailText = let e = T.unpack emailText in
		fmapL (\err -> "Error parsing email <" ++ e ++ ">\n" ++ show err)
			(EmailAddress.validate e)

createChoice root db gid req = eitherT errorPage return $ do
	(body, _) <- lift $ first (map (T.decodeUtf8 *** T.decodeUtf8)) <$> parseRequestBody noStoreFileUploads req
	(email, choice) <- hoistEither $ parseCreateChoiceRequest body
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
			mailBody <- responseToMailPart True $ ResponseBuilder ok200 [] (viewEmail id context)
			scriptIO $ renderSendMail Mail {
					mailFrom    = appEmail,
					mailTo      = [emailToAddress (fst a), emailToAddress email],
					mailCc      = [], mailBcc  = [],
					mailHeaders = [(fromString "Subject", T.pack $ "[Roshambo "++gid++"] " ++ winTxt)],
					mailParts   = [[mailBody]]
				}
		_ -> throwT "You cannot make a new choice on a completed game!"
	redirect' seeOther303 [] (uriRelativeTo root $ showGamePath gid)
	where
	emailToAddress = Address Nothing . T.pack . show
