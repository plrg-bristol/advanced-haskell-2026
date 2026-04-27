
{-# LANGUAGE InstanceSigs #-}

module Comonad where

import Prelude hiding (Monad(..))

class (Functor m, Applicative m) => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  -- alternative to bind
  join :: m (m a) -> m a

  -- join and bind can be defined in terms of each other
  join mma = mma >>= id
  -- last time we came up with this:
  -- join mma = mma >>= (\ma -> ma >>= return)
  -- which is the same by the right id law
  ma >>= k = join (fmap k ma)
    -- k :: a -> m b
    -- ma :: m a
    -- also have fmap
    -- and applicatives

-- monad laws
-- with bind:
-- Left identity: return a >>=	 h ≡ h a
-- Right identity:	m >>=	 return  ≡ m
-- Associativity: (m >>= g) >>= h ≡ m >>= (\x -> g x >>= h)
-- with join:
-- fmap g . return = return . g
-- fmap g . join = join . fmap (fmap g)
-- join . fmap join = join . join
-- join . return = id = join . fmap return

class Functor w => Comonad w where
  extract :: w e -> e
  duplicate :: w e -> w (w e)
  extend :: (w d -> e) -> w d -> w e

  duplicate we = extend id we
  extend k wd = fmap k $ duplicate wd

-- comonad laws
-- extend extract      = id
-- extract . extend f  = f
-- extend f . extend g = extend (f . extend g)
-- extract . duplicate      = id
-- fmap extract . duplicate = id
-- duplicate . duplicate    = fmap duplicate . duplicate

data Infinite a = Cons a (Infinite a) deriving Show

exampleInf :: Infinite Int
exampleInf = Cons 1 (Cons 2 (Cons 3 exampleInf))

instance Functor Infinite where
  fmap :: (a -> b) -> Infinite a -> Infinite b
  fmap f (Cons a i) = Cons (f a) (fmap f i)

data Tape a = Tape (Infinite a) a (Infinite a) deriving Show

instance Functor Tape where
  fmap :: (a -> b) -> Tape a -> Tape b
  fmap f (Tape l a r) = Tape (fmap f l) (f a) (fmap f r)

-- fmap laws
--Identity
-- fmap id == id
-- Composition
-- fmap (f . g) == fmap f . fmap g

instance Comonad Tape where
  extract :: Tape a -> a
  extract (Tape _ a _) = a

  duplicate :: Tape a -> Tape (Tape a)
  duplicate ta = Tape undefined ta undefined

-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfold :: (a -> (a,a)) -> a -> Infinite a
unfold f a =
  let (a1, a2) = f a
  in Cons a1 (unfold f a2)

allLefts :: Tape a -> Infinite (Tape a)
allLefts t = unfold undefined undefined

helperL :: Tape a -> Tape a
helperL (Tape (Cons x xs) a rs) = Tape xs x (Cons a rs)

helperR :: Tape a -> Tape a
helperR (Tape ls a (Cons x xs)) = Tape (Cons a ls) x xs
