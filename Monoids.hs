module Monoid where

import Prelude hiding (Semigroup(..), Monoid(..), sum, product)

-- monoids - group without inverse

-- semigroup
-- ----------

-- < set (type), op>
-- Int, +

-- monoids
-- -------
-- < set (type), op, id >
-- Int, +, 0

-- typeclasses

class Semigroup a where
  (<>) :: a -> a -> a

class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mappend = (<>)

-- Int, +, 0

instance Semigroup Int where
  x <> y = x + y
  -- (<>) = (+)

instance Monoid Int where
  mempty = 0

-- groups
-- -------
-- < set (type), op, id, inv>
-- Int, +, 0, negate

-- monoid laws
-- mempty <> x == x -- left identity
-- x <> mempty == x -- right identity
-- we have both cos <> doesnt need to be commutative i.e. x <> y /= y <> x
-- e.g. String, ++, "" is not comm
-- (x <> y) <> z == x <> (y <> z)

-- Monoids and folds: mconcat
-- -----------------------------

sum :: Num a => [a] -> a
sum = foldr (+) 0

product :: Num a => [a] -> a
product = foldr (*) 1

mconcat :: Monoid m => [m] -> m
mconcat = foldr mappend mempty

-- ...

-- Monoid homomorphisms
-- Difference Lists
-- Monoid actions
  -- ... monoid theme and variation
-- free monoid

-- nick zoo paper