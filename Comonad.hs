
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
  extend :: w d -> (w d -> e) -> w e

  duplicate we = extend we id
  extend = undefined