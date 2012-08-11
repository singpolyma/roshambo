module Application where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Numeric (showHex)
import Data.Char
import Data.Word
import Data.List
import System.Random (randomR, getStdRandom)
import Data.String

import Network.Wai
import Network.HTTP.Types (ok200, notFound404, seeOther303, badRequest400, parseQueryText, Status, ResponseHeaders)
import Data.Conduit (($$), runResourceT, ResourceT)
import Data.Conduit.List (fold)

import Text.Hastache (hastacheFileBuilder, MuConfig(..), MuType(..), MuContext)
import qualified Text.Hastache as Hastache (htmlEscape)
import qualified Text.Hastache.Context as Hastache (mkStrContext)

import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LZ

import Data.Monoid (mappend, mempty)

import Database

hastache :: (Functor m, MonadIO m) => Status -> ResponseHeaders -> FilePath -> MuContext m -> m Response
hastache status headers pth ctx = fmap (ResponseBuilder status headers) (
		hastacheFileBuilder
			(MuConfig Hastache.htmlEscape Nothing (Just "mustache")) pth ctx
	)

-- TODO require absolute URI for uri with types?
redirect :: (Monad m) => Status -> ResponseHeaders -> ByteString -> m Response
redirect status headers uri = return $ responseLBS status ((fromString "Location", uri):headers) mempty

stringAscii :: (IsString s) => String -> Maybe s
stringAscii s
	| all isAscii s = Just (fromString s)
	| otherwise     = Nothing

s :: (IsString s) => String -> s
s istr = let Just str = stringAscii istr in str

stringHeaders :: (IsString s1, IsString s2) => [(String, String)] -> Maybe [(s1, s2)]
stringHeaders = sequence . map (\(n,v) ->
		liftM2 (,) (stringAscii n) (stringAscii v)
	)

stringHeaders' :: (IsString s1, IsString s2) => [(String, String)] -> [(s1, s2)]
stringHeaders' hs = let Just headers = stringHeaders hs in headers

textToUTF8 txt = LZ.fromChunks [T.encodeUtf8 txt]

showUTF8 :: (Show a) => a -> LZ.ByteString
showUTF8 = textToUTF8 . T.pack . show

on404 _ = return $ responseLBS notFound404 (stringHeaders' [("Content-Type", "text/plain")]) (s"Not Found")

bodyBytestring req = liftIO $ runResourceT $ requestBody req $$ fold mappend mempty

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
	hastache ok200 (stringHeaders' [("Content-Type", "text/html")]) "rps.mustache" context
	where
	rpsWinner Rock     Paper    = 0
	rpsWinner Paper    Rock     = 1
	rpsWinner Rock     Scissors = 0
	rpsWinner Scissors Rock     = 1
	rpsWinner Scissors Paper    = 0
	rpsWinner Paper    Scissors = 1

	ctx (Just (RPSGameStart a))    "first"  = MuVariable $ show a
	ctx (Just (RPSGameFinish a _)) "first"  = MuVariable $ show a
	ctx (Just (RPSGameFinish _ b)) "second" = MuVariable $ show b
	ctx (Just (RPSGameFinish a b)) "winner" = MuVariable $ "Player " ++ show (rpsWinner a b + 1)
	ctx _ _ = MuNothing

createChoice db id req = do
	body <- fmap parseQueryText (bodyBytestring req)
	case join $ lookup (T.pack "choice") body of
		Nothing ->
			return $ responseLBS badRequest400 (stringHeaders' [("Content-Type", "text/plain")]) (s"You didn't send a choice!")
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
