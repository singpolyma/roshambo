module Application where

import Data.Char
import Data.Word
import Data.Maybe
import Data.List
import Control.Monad
import Control.Arrow
import Numeric (showHex)
import Data.Monoid (mappend, mempty)
import Data.String (IsString, fromString)
import Control.Error (eitherT, throwT, note, liftEither, fmapL, tryRead, EitherT(..))
import Control.Exception (try, SomeException)
import Control.Monad.Trans (MonadIO, liftIO, lift)
import System.Random (randomR, getStdRandom)

import Network.Wai (Request(..), Response(..), responseLBS, responseSource)
import Network.Wai.Parse (parseRequestBody, BackEnd, parseHttpAccept)
import Network.HTTP.Types (ok200, notFound404, seeOther303, badRequest400, notAcceptable406, statusIsRedirection, Status(..), ResponseHeaders, Header)
import Data.Conduit (($$), runResourceT, Flush(..), ResourceT)
import Data.Conduit.List (fold, sinkNull)

import Network.Wai.Hastache (hastacheHTML, hastacheText, MuType(..), MuContext)
import qualified Text.Hastache as Hastache (decodeStr)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (split)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Text.Email.Validate (EmailAddress)
import qualified Text.Email.Validate as EmailAddress (validate)
import Network.URI (URI(..), URIAuth(..))
import qualified Network.URI as URI
import Network.Mail.Mime
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Blaze.ByteString.Builder as Builder
import qualified Blaze.ByteString.Builder.Char.Utf8 as Builder
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

string :: (MonadIO m) => Status -> ResponseHeaders -> String -> m Response
string status headers = return . defHeader defCT . ResponseBuilder status headers . Builder.fromString
	where
	Just defCT = stringHeader ("Content-Type", "text/plain; charset=utf-8")

text :: (MonadIO m) => Status -> ResponseHeaders -> T.Text -> m Response
text status headers = return . defHeader defCT . ResponseBuilder status headers . Builder.fromText
	where
	Just defCT = stringHeader ("Content-Type", "text/plain; charset=utf-8")

json :: (MonadIO m, Aeson.ToJSON a) => Status -> ResponseHeaders -> a -> m Response
json status headers = return . defHeader defCT . responseLBS status headers . Aeson.encode . Aeson.toJSON
	where
	Just defCT = stringHeader ("Content-Type", "application/json; charset=utf-8")

uriIsAbsolute :: URI -> Bool
uriIsAbsolute (URI {
		uriScheme = scheme,
		uriAuthority = Just (URIAuth {uriRegName = reg})
	}) = scheme /= "" && reg /= ""
uriIsAbsolute _ = False

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
bodyBytestring req = requestBody req $$ fold mappend mempty

buildURI :: URI -> String -> Maybe URI
buildURI root rel =
	URI.parseRelativeReference rel >>= (`URI.relativeTo` root)

buildURI' :: URI -> String -> URI
buildURI' root rel = let Just uri = buildURI root rel in uri

on404 _ = string notFound404 [] "Not Found"

appEmail :: Address
appEmail = Address (Just $ T.pack "Roshambo App") (T.pack "roshambo@example.com")

errorPage :: (MonadIO m, Functor m) => String -> m Response
errorPage msg = hastacheHTML badRequest400 [] "error.mustache" context
	where
	context = ctxToMuContext [("errorMessage", CtxString msg)]

rpsWinner :: (EmailAddress, RPSChoice) -> (EmailAddress, RPSChoice) -> Maybe EmailAddress
rpsWinner (e,Paper)    (_,Rock)     = Just e
rpsWinner (_,Rock)     (e,Paper)    = Just e
rpsWinner (e,Rock)     (_,Scissors) = Just e
rpsWinner (_,Scissors) (e,Rock)     = Just e
rpsWinner (e,Scissors) (_,Paper)    = Just e
rpsWinner (_,Paper)    (e,Scissors) = Just e
rpsWinner _ _ = Nothing

data CtxVar =
	CtxString String |
	CtxBool Bool |
	CtxList [[(String, CtxVar)]]
	deriving (Eq)

instance Show CtxVar where
	show (CtxString s) = s
	show (CtxBool b) = show b
	show (CtxList xs) = show xs

instance Aeson.ToJSON CtxVar where
	toJSON (CtxString s) = Aeson.toJSON s
	toJSON (CtxBool b) = Aeson.toJSON b
	toJSON (CtxList xs) = Aeson.toJSON $ map (Aeson.object . map ctxPair) xs

ctxPair :: (String, CtxVar) -> Aeson.Pair
ctxPair (k, v) = (T.pack k, Aeson.toJSON v)

ctxChoice :: (EmailAddress,RPSChoice) -> [(String, CtxVar)]
ctxChoice (e,c) =
	[("email", CtxString $ show e), ("choice", CtxString $ show c)]

ctx :: Maybe RPSGame -> [(String, CtxVar)]
ctx (Just (RPSGameStart a)) = [("choices", CtxList [ctxChoice a])]
ctx (Just (RPSGameFinish a b)) = winner ++ [
		("choices", CtxList [ctxChoice a, ctxChoice b]),
		("finished", CtxBool True)
	]
	where
	winner = case rpsWinner a b of
		Just e -> [("winner", CtxString $ show e)]
		Nothing -> []
ctx _ = []

ctxToMuContext :: (Monad m) => [(String, CtxVar)] -> MuContext m
ctxToMuContext xs =
	ctxToMuType . (`lookup` xs) . Hastache.decodeStr
	where
	ctxToMuType (Just (CtxString s)) = MuVariable s
	ctxToMuType (Just (CtxBool s)) = MuBool s
	ctxToMuType (Just (CtxList xs)) = MuList $ map ctxToMuContext xs
	ctxToMuType Nothing = MuNothing

ctxToAeson :: [(String, CtxVar)] -> Aeson.Value
ctxToAeson = Aeson.object . map ctxPair

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

showGame _ db id req = do
	context <- fmap ctx $ dbGet db id
	case acceptType of
		"text/html" ->
			hastacheHTML ok200 [] "rps.mustache" (ctxToMuContext context)
		"application/json" ->
			json ok200 [] (ctxToAeson $ filter ((/="choices") . fst) context)
		_ -> string notAcceptable406 [] (intercalate "\n" supportedTypes)
	where
	acceptType = fromMaybe (head supportedTypes) acceptType'
	acceptType' = (selectAcceptType supportedTypes . parseHttpAccept) =<<
		lookup (fromString "Accept") (requestHeaders req)
	supportedTypes = ["text/html", "application/json"]

tryIO :: (MonadIO m, Functor m) => IO a -> EitherT String m a
tryIO io =
	EitherT . fmap (fmapL show) $ liftIO $ (try :: IO a -> IO (Either SomeException a)) io

createChoice root db id req = eitherT errorPage return $ do
	(body', _) <- lift $ parseRequestBody noStoreFileUploads req
	let body = map (T.decodeUtf8 *** T.decodeUtf8) body'
	email <- tryParseEmail =<< tryEmailParam body
	choice <- maybeMsg "You didn't send a choice!" $ param body "choice"
	v <- tryIO $ dbGet db id
	case v of
		Nothing -> (tryIO . dbSet db id) =<< (\c -> RPSGameStart (email,c)) `fmap` tryReadRPS choice
		Just (RPSGameStart a) -> do
			b <- fmap ((,)email) $ tryReadRPS choice
			let rpsGame = RPSGameFinish a b
			let context = ctx (Just rpsGame)
			let winTxt = case lookup "winner" context of
				Just e -> show e ++ " wins!"
				Nothing -> "It's a tie!"
			tryIO $ dbSet db id rpsGame
			mailBody <- responseToMailPart True =<< hastacheText ok200 [] "email.mustache" (ctxToMuContext context)
			tryIO $ renderSendMail Mail {
					mailFrom    = appEmail,
					mailTo      = [emailToAddress (fst a), emailToAddress (fst b)],
					mailCc      = [], mailBcc  = [],
					mailHeaders = [(fromString "Subject", T.pack $ "[Roshambo "++id++"] " ++ winTxt)],
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
