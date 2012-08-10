module Application where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Numeric (showHex)
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

s :: (IsString a) => String -> a
s = fromString

hastache :: (Functor m, MonadIO m) => Status -> ResponseHeaders -> FilePath -> MuContext m -> m Response
hastache status headers pth ctx = fmap (ResponseBuilder status headers) (
		hastacheFileBuilder
			(MuConfig Hastache.htmlEscape Nothing (Just "mustache")) pth ctx
	)

-- TODO require absolute URI for uri with types?
redirect :: (Monad m) => Status -> ResponseHeaders -> ByteString -> m Response
redirect status headers uri = return $ responseLBS status ((s"Location", uri):headers) mempty

textToUTF8 txt = LZ.fromChunks [T.encodeUtf8 txt]

showUTF8 :: (Show a) => a -> LZ.ByteString
showUTF8 = textToUTF8 . T.pack . show

on404 _ = return $ responseLBS notFound404 [(s"Content-Type", s"text/plain")] (s"Not Found")

bodyBytestring req = liftIO $ runResourceT $ (requestBody req) $$ fold mappend mempty

home db _ = do
	id <- uniqId
	return $ responseLBS seeOther303 [(s"Location", T.encodeUtf8 $ T.pack $ "/game/" ++ id)] mempty
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
	-- TODO: probably just always show form unless there's a winner
	v <- dbGet db id
	case v of
		Nothing ->
			return $ responseLBS ok200 [(s"Content-Type", s"text/plain")] (s"No one has entered yet")
		Just choice ->
			return $ responseLBS ok200 [(s"Content-Type", s"text/plain")] (showUTF8 choice)

createChoice db id req = do
	body <- fmap parseQueryText (bodyBytestring req)
	case join $ lookup (T.pack "choice") body of
		Nothing ->
			return $ responseLBS badRequest400 [(s"Content-Type", s"text/plain")] (s"You didn't send a choice!")
		Just choice -> do
			v <- dbGet db id
			case v of
				Nothing -> do
					dbSet db id (T.unpack choice)
					return $ responseLBS ok200 [(s"Content-Type", s"text/plain")] (s"Set!")
				Just otherChoice -> error "TODO: choose winner"
