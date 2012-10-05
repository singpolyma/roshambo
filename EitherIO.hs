-- | A wrapper around 'IO' and 'EitherT' to get rid of exceptions
--
-- If you type-restrict something to 'EitherIO' then any exceptions or
-- other errors will have to be caught explicitly or else the compiler
-- will complain.
--
-- Only 'IOError's are caught.  No other sort of exception should be caught
-- in normal code (since they either represent programmer error or program or
-- thread termination).  The one exception is 'bracketEitherIO', which will
-- ensure that the cleanup routine is run even if another exception occurs.
module EitherIO (EitherIO, eitherIO, handleEitherIO, bracketEitherIO, runEitherIO, hoistIO, failIO, MonadIO(..)) where

import Prelude hiding (catch)
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Control.Exception
import Control.Error

-- | Using 'liftIO' into 'EitherIO' catches IO exceptions
newtype EitherIO a = EitherIO (EitherT IOError IO a)

instance MonadIO EitherIO where
	liftIO = EitherIO . tryIO

instance Functor EitherIO where
	fmap f (EitherIO v) = EitherIO (fmap f v)

instance Applicative EitherIO where
	pure = EitherIO . pure
	(EitherIO x) <*> (EitherIO y) = EitherIO (x <*> y)

instance Monad EitherIO where
	return = pure
	(EitherIO x) >>= f = EitherIO (x >>= (\v -> let EitherIO r = f v in r))
	fail s = EitherIO $ left (userError s)

-- | fail lifted to 'MonadIO' so you don't accidentally use it in a
-- Monad with no implementation
failIO :: (MonadIO m) => String -> m a
failIO = liftIO . fail

-- | Run 'EitherIO' and render errors as an 'Either'
runEitherIO :: (MonadIO m) => EitherIO a -> m (Either IOError a)
runEitherIO (EitherIO v) = liftIO $ runEitherT v

-- | Run EitherIO, handling any errors with the given handler
handleEitherIO :: (MonadIO m) => (IOError -> m a) -> EitherIO a -> m a
handleEitherIO handler action =
	runEitherIO action >>= either handler (liftIO . return)

-- | Hoist an 'IO' action with a potential error as an 'Either' into 'EitherIO'
hoistIO :: IO (Either IOError a) -> EitherIO a
hoistIO = liftIO >=> (EitherIO . hoistEither)

-- | Run the first action if there is an error, or the second action otherwise
eitherIO :: (IOError -> EitherIO b) -> (a -> EitherIO b) -> EitherIO a -> EitherIO b
eitherIO lft rht = join . liftIO . fmap (either lft rht) . runEitherIO

-- Safely acquire a resource, use it, and release it, handling errors in
-- 'EitherIO'
bracketEitherIO :: EitherIO a -> (a -> EitherIO b) -> (a -> EitherIO c) -> EitherIO c
bracketEitherIO acquire release body = hoistIO bracket'
	where
	bracket' = mask $ \restore -> do
		a <- acquire' -- Left from this will get bubbled by body'/release'
		-- IOExceptions are getting caught in body', onException will only
		-- fire for other kinds of exceptions (most notably, async)
		b <- restore (body' a) `onException` release' a
		r <- release' a
		-- Could have used bracket directly, but then errors from release
		-- get thrown out (they're expected to be thrown as exception),
		-- so do this:
		return $ case (b, r) of
			(Left x, _) -> Left x -- Prefer earlier error
			(_, Left x) -> Left x -- But do bubble later error
			_ -> b
	acquire' = runEitherIO acquire
	release' (Left x) = return $ Left x
	release' (Right x) = runEitherIO (release x)
	body' (Left x) = return $ Left x
	body' (Right x) = runEitherIO (body x)
