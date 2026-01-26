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

  -- Two possible behaviours:
  (<*>) :: MaybeIO (a -> b) -> MaybeIO a -> MaybeIO b
  -- Behaviour 1: If the first MaybeIO returns Nothing, short-circuit and don't run any more IO
  (MaybeIO ioF) <*> (MaybeIO ioA) = MaybeIO $ do
    mf <- ioF
    case mf of
      Nothing -> pure Nothing
      Just f -> do
        ma <- ioA
        pure (fmap f ma)
  
  -- Behaviour 2: Run all IO, regardless of returning Nothing, and then combine `Maybe`s after.
  -- (MaybeIO ioF) <*> (MaybeIO ioA) = MaybeIO (liftA2 (<*>) ioF ioA)
  -- (MaybeIO ioF) <*> (MaybeIO ioA) = MaybeIO $ do
  --   mf <- ioF
  --   ma <- ioA
  --   pure (mf <*> ma)
    -- pure $ do
    --   f <- mf
    --   a <- ma
    --   pure (f a)
    -- pure $ 
    --   mf >>= (\f -> 
    --   ma >>= (\a -> 
    --   pure (f a)))

-- Only Behaviour 1 of the Applicative instance is compatible with the Monad instance.
-- For Behaviour 2, if the `MaybeIO a` returns `Nothing`, where can you get the `a` from
-- for the `(a -> MaybeIO b)`?
instance Monad MaybeIO where
  (>>=) :: MaybeIO a -> (a -> MaybeIO b) -> MaybeIO b
  (MaybeIO ioMa) >>= fMIOb = MaybeIO $ do
    ma <- ioMa
    case ma of
      Nothing -> pure Nothing
      Just a -> do
        let MaybeIO mIOb = fMIOb a
        mIOb

safeReadFile :: FilePath -> MaybeIO String
safeReadFile file = MaybeIO $ do
  putStrLn ("Attempting to read file: " ++ file)
  exists <- fileExist file
  if exists then do
    f <- readFile file
    pure (Just f)
  else
    pure Nothing

safeReadTwo :: FilePath -> FilePath -> MaybeIO (String, String)
safeReadTwo file1 file2 = do
  file1Contents <- safeReadFile file1
  file2Contents <- safeReadFile file2
  pure (file1Contents, file2Contents)

-- Bonus

runMaybeIO :: MaybeIO a -> IO (Maybe a)
runMaybeIO (MaybeIO mx) = mx

-- Try running these two expressions in ghci:

-- ghci> runMaybeIO (safeReadTwo ".gitignore" "non-existent.txt")
-- ghci> runMaybeIO (safeReadTwo "non-existent.txt" ".gitignore")

