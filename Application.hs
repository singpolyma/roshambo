module Application where

import Data.Char
import Data.Word
import Data.Maybe
import Control.Monad
import Control.Arrow
import Numeric (showHex)
import Data.Monoid (mappend, mempty)
import Data.String (IsString, fromString)
import Control.Error (eitherT, throwT, note, liftEither, fmapL, tryRead, EitherT)
import Control.Monad.Trans (MonadIO, liftIO, lift)
import System.Random (randomR, getStdRandom)

import Network.Wai (Request(..), Response(..), responseLBS, responseSource)
import Network.Wai.Parse (parseRequestBody, BackEnd)
import Network.HTTP.Types (ok200, notFound404, seeOther303, badRequest400, Status(..), ResponseHeaders, Header)
import Data.Conduit (($$), runResourceT, Flush(..), ResourceT)
import Data.Conduit.List (fold, sinkNull)

import Text.Hastache (hastacheFileBuilder, MuConfig(..), MuType(..), MuContext)
import qualified Text.Hastache as Hastache (htmlEscape)
import qualified Text.Hastache.Context as Hastache (mkStrContext)

import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Text.Email.Validate (EmailAddress)
import qualified Text.Email.Validate as EmailAddress (validate)
import Network.URI (URI(..), URIAuth(..))
import qualified Network.URI as URI
import Network.Mail.Mime
import qualified Blaze.ByteString.Builder as Builder
import qualified Data.CaseInsensitive as CI

import Database

noStoreFileUploads :: BackEnd ()
noStoreFileUploads _ _ = sinkNull

maybeMsg :: (Monad m) => a -> Maybe b -> EitherT a m b
maybeMsg msg = liftEither . note msg

maybeBlank :: T.Text -> Maybe T.Text
maybeBlank t
	| T.null t  = Nothing
	| otherwise = Just t

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

hastache :: (Functor m, MonadIO m) => Status -> ResponseHeaders -> FilePath -> MuContext m -> m Response
hastache status headers pth ctx = fmap (ResponseBuilder status headers) (
		hastacheFileBuilder
			(MuConfig Hastache.htmlEscape Nothing (Just "mustache")) pth ctx
	)

text :: (MonadIO m) => Status -> ResponseHeaders -> TL.Text -> m Response
text status headers = return . defHeader defCT . responseLBS status headers . TL.encodeUtf8
	where
	Just defCT = stringHeader ("Content-Type", "text/plain; charset=utf-8")

string :: (MonadIO m) => Status -> ResponseHeaders -> String -> m Response
string status headers = text status headers . TL.pack

statusIsRedirect :: Status -> Bool
statusIsRedirect (Status {statusCode=code}) = code >= 300 && code < 400

uriIsAbsolute :: URI -> Bool
uriIsAbsolute (URI {
		uriScheme = scheme,
		uriAuthority = Just (URIAuth {uriRegName = reg})
	}) = scheme /= "" && reg /= ""
uriIsAbsolute _ = False

redirect :: Status -> ResponseHeaders -> URI -> Maybe Response
redirect status headers uri
	| statusIsRedirect status && uriIsAbsolute uri = do
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

bodyBytestring :: Request -> ResourceT IO ByteString
bodyBytestring req = requestBody req $$ fold mappend mempty

buildURI :: URI -> String -> Maybe URI
buildURI root rel =
	URI.parseRelativeReference rel >>= (`URI.relativeTo` root)

buildURI' :: URI -> String -> URI
buildURI' root rel = let Just uri = buildURI root rel in uri

on404 _ = string notFound404 [] "Not Found"

appEmail :: Address
appEmail = Address (Just $ T.pack "Roshambo App") (T.pack "roshambo@example.com")

errorPage :: (MonadIO m) => String -> m Response
errorPage = string badRequest400 []

rpsWinner :: (EmailAddress, RPSChoice) -> (EmailAddress, RPSChoice) -> Maybe EmailAddress
rpsWinner (e,Paper)    (_,Rock)     = Just e
rpsWinner (_,Rock)     (e,Paper)    = Just e
rpsWinner (e,Rock)     (_,Scissors) = Just e
rpsWinner (_,Scissors) (e,Rock)     = Just e
rpsWinner (e,Scissors) (_,Paper)    = Just e
rpsWinner (_,Paper)    (e,Scissors) = Just e
rpsWinner _ _ = Nothing

ctx :: Maybe RPSGame -> String -> Maybe String
ctx (Just (RPSGameStart  (_,a)  )) "first"  = Just $ show a
ctx (Just (RPSGameFinish (_,a) _)) "first"  = Just $ show a
ctx (Just (RPSGameFinish _ (_,b))) "second" = Just $ show b
ctx (Just (RPSGameFinish a b))     "winner" = Just $ case rpsWinner a b of
	Nothing -> "It's a tie!"
	Just e  -> show e ++ " wins!"
ctx _ _ = Nothing

ctxToContext :: (Monad m) => (String -> Maybe String) -> MuContext m
ctxToContext f = Hastache.mkStrContext g
	where
	g k = case f k of
		Just s  -> MuVariable s
		Nothing -> MuNothing

home root db _ = do
	id <- uniqId
	redirect' seeOther303 [] (buildURI' root ("/game/" ++ id))
	where
	-- Can't liftIO the do block directly because of the loop
	uniqId = liftIO uniqId'
	uniqId' = do
		id <- fmap (`showHex` "") $ getStdRandom (randomR (minBound,maxBound :: Word32))
		v <- dbGet db id
		case v of
			Nothing -> return id
			_ -> uniqId'

showGame _ db id _ = do
	context <- fmap (ctxToContext . ctx) $ dbGet db id
	hastache ok200 (stringHeaders' [("Content-Type", "text/html; charset=utf-8")]) "rps.mustache" context

createChoice root db id req = eitherT errorPage return $ do
	(body', _) <- lift $ parseRequestBody noStoreFileUploads req
	let body = map (T.decodeUtf8 *** T.decodeUtf8) body'
	email <- tryParseEmail =<< tryEmailParam body
	choice <- maybeMsg "You didn't send a choice!" $ param body "choice"
	v <- dbGet db id
	case v of
		Nothing -> dbSet db id =<< (\c -> RPSGameStart (email,c)) `fmap` tryReadRPS choice
		Just (RPSGameStart a) -> do
			b <- fmap ((,)email) $ tryReadRPS choice
			let rpsGame = RPSGameFinish a b
			let context = ctx (Just rpsGame)
			dbSet db id rpsGame
			mailBody <- responseToMailPart True =<< hastache ok200 [] "email.mustache" (ctxToContext context)
			liftIO $ renderSendMail Mail {
					mailFrom    = appEmail,
					mailTo      = [emailToAddress (fst a), emailToAddress (fst b)],
					mailCc      = [], mailBcc  = [],
					mailHeaders = [(fromString "Subject", T.pack $ "[Roshambo "++id++"] " ++ fromJust (context "winner"))],
					mailParts   = [[mailBody]]
				}
		_ -> throwT "You cannot make a new choice on a completed game!"
	redirect' seeOther303 [] (buildURI' root ("/game/" ++ id))
	where
	emailToAddress = Address Nothing . T.pack . show
	param body k = lookup (T.pack k) body
	tryReadRPS c = let c' = T.unpack c in
		tryRead ("\""++c'++"\" is not one of: Rock, Paper, Scissors") c'
	tryEmailParam body = maybeMsg "You didn't send an email address!"
		(fmap T.unpack $ maybeBlank =<< param body "email")
	tryParseEmail email = liftEither $
		fmapL (\err -> "Error parsing email <" ++ email ++ ">\n" ++ show err)
			(EmailAddress.validate email)
