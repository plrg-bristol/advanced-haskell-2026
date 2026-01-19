module MonadTransformers where

import System.Directory.Extra
import System.Posix.Files

foo :: FilePath -> IO String
foo file = do
  exists <- fileExist file
  if exists then do
    f <- readFile file
    pure f
  else
    pure ""

safeAdd :: Maybe Int -> Maybe Int -> Maybe Int
safeAdd maybeX maybeY = do
  x <- maybeX
  y <- maybeY
  pure (x + y)

-- safeFile :: Maybe (IO String)
-- safeFile :: IO (Maybe String)

newtype MaybeIO a = MaybeIO (IO (Maybe a))

instance Functor MaybeIO where
  fmap :: (a -> b) -> MaybeIO a -> MaybeIO b
  fmap f (MaybeIO ioMa) = MaybeIO (fmap (fmap f) ioMa)

instance Applicative MaybeIO where
  pure :: a -> MaybeIO a
  pure x = MaybeIO (pure (pure x))
