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
import Network.HTTP.Types (ok200, notFound404, seeOther303, badRequest400, parseQueryText, Status, ResponseHeaders, Header)
import Data.Conduit (($$), runResourceT)
import Data.Conduit.List (fold)

import Text.Hastache (hastacheFileBuilder, MuConfig(..), MuType(..), MuContext)
import qualified Text.Hastache as Hastache (htmlEscape)
import qualified Text.Hastache.Context as Hastache (mkStrContext)

import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

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

-- TODO require absolute URI for uri with types?
-- require that (statusCode status) >= 300 && < 400 ?
redirect :: (Monad m) => Status -> ResponseHeaders -> ByteString -> m Response
redirect status headers uri = return $ responseLBS status ((location, uri):headers) mempty
	where
	Just location = stringAscii "Location"

stringAscii :: (IsString s) => String -> Maybe s
stringAscii s
	| all isAscii s = Just (fromString s)
	| otherwise     = Nothing

stringHeader :: (IsString s1, IsString s2) => (String, String) -> Maybe (s1, s2)
stringHeader (n, v) = liftM2 (,) (stringAscii n) (stringAscii v)

stringHeaders :: (IsString s1, IsString s2) => [(String, String)] -> Maybe [(s1, s2)]
stringHeaders = sequence . map stringHeader

stringHeaders' :: (IsString s1, IsString s2) => [(String, String)] -> [(s1, s2)]
stringHeaders' hs = let Just headers = stringHeaders hs in headers

bodyBytestring :: (MonadIO m) => Request -> m ByteString
bodyBytestring req = liftIO $ runResourceT $ requestBody req $$ fold mappend mempty

on404 _ = string notFound404 [] "Not Found"

home db _ = do
	id <- uniqId
	redirect seeOther303 [] (T.encodeUtf8 $ T.pack $ "/game/" ++ id)
	where
	-- Can't liftIO the do block directly because of the loop
	uniqId = liftIO uniqId'
	uniqId' = do
		id <- fmap (`showHex` "") $ getStdRandom (randomR (minBound,maxBound :: Word32))
		v <- dbGet db id
		case v of
			Nothing -> return id
			_ -> uniqId'

showGame db id _ = do
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

createChoice db id req = do
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
			redirect seeOther303 [] (T.encodeUtf8 $ T.pack $ "/game/" ++ id)
