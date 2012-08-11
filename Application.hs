module Application where

import Data.Char
import Data.Word
import Control.Monad
import Numeric (showHex)
import Data.Monoid (mappend, mempty)
import Data.String (IsString, fromString)
import Control.Monad.Trans (MonadIO, liftIO)
import System.Random (randomR, getStdRandom)

import Network.Wai (Request(..), Response(..), responseLBS)
import Network.HTTP.Types (ok200, notFound404, seeOther303, badRequest400, parseQueryText, Status(..), ResponseHeaders, Header)
import Data.Conduit (($$), runResourceT)
import Data.Conduit.List (fold)

import Text.Hastache (hastacheFileBuilder, MuConfig(..), MuType(..), MuContext)
import qualified Text.Hastache as Hastache (htmlEscape)
import qualified Text.Hastache.Context as Hastache (mkStrContext)

import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Network.URI (URI(..), URIAuth(..))
import qualified Network.URI as URI

import Database

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

bodyBytestring :: (MonadIO m) => Request -> m ByteString
bodyBytestring req = liftIO $ runResourceT $ requestBody req $$ fold mappend mempty

buildURI :: URI -> String -> Maybe URI
buildURI root rel =
	URI.parseRelativeReference rel >>= (`URI.relativeTo` root)

buildURI' :: URI -> String -> URI
buildURI' root rel = let Just uri = buildURI root rel in uri

on404 _ = string notFound404 [] "Not Found"

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
	context <- fmap (Hastache.mkStrContext . ctx) $ dbGet db id
	hastache ok200 (stringHeaders' [("Content-Type", "text/html; charset=utf-8")]) "rps.mustache" context
	where
	rpsWinner :: RPSChoice -> RPSChoice -> Int
	rpsWinner Paper    Rock     = 0
	rpsWinner Rock     Paper    = 1
	rpsWinner Rock     Scissors = 0
	rpsWinner Scissors Rock     = 1
	rpsWinner Scissors Paper    = 0
	rpsWinner Paper    Scissors = 1
	rpsWinner _ _ = -1 -- TODO Deal with ties

	ctx (Just (RPSGameStart a))    "first"  = MuVariable $ show a
	ctx (Just (RPSGameFinish a _)) "first"  = MuVariable $ show a
	ctx (Just (RPSGameFinish _ b)) "second" = MuVariable $ show b
	ctx (Just (RPSGameFinish a b)) "winner" = MuVariable $ "Player " ++ show (rpsWinner a b + 1)
	ctx _ _ = MuNothing

createChoice root db id req = do
	body <- fmap parseQueryText (bodyBytestring req)
	case join $ lookup (T.pack "choice") body of
		Nothing ->
			string badRequest400 [] "You didn't send a choice!"
		Just choice -> do
			v <- dbGet db id
			case v of
				Nothing ->
					dbSet db id (RPSGameStart $ read $ T.unpack choice) -- TODO: handle bad input
				Just (RPSGameStart otherChoice) ->
					dbSet db id (RPSGameFinish otherChoice $ read $ T.unpack choice) -- TODO: handle bad input
				_ ->
					return () -- TODO: error page. You cannot make a choice, there is a winner
			redirect' seeOther303 [] (buildURI' root ("/game/" ++ id))
