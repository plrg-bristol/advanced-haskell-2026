module Monoid where

import Prelude hiding (Semigroup(..), Monoid(..), sum, product, length, reverse)

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


-- []

instance Semigroup [a] where
  (<>) = (++)
instance Monoid [a] where
  mempty = []
  -- mappend = (++)

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
--    f mempty_1 = mempty_2
--    f (x <>_1 y) = f x <>_2 f y

length :: [a] -> Int
length = foldr (\_ l -> l+1 ) 0

-- Difference Lists


-- endomorphism monoid
-- <a->a, (.), id>

instance Semigroup (a->a) where
  f <> g = f . g

instance Monoid (a->a) where
  mempty = id

reverse :: [a] -> [a]
reverse [] = []
-- reverse [x] = [x]
reverse (x:xs) = (reverse xs) ++ [x]

plusplus :: [a] -> [a] -> [a]
plusplus [] ys = ys
plusplus (x:xs) ys = x : xs ++ ys

{-

https://v1.sflexplorer.kiransturt.co.uk/

-- reverse [1,2,3]
-- (reverse [2,3]) ++ [1]
-- ((reverse [3]) ++ [2]) ++ [1]
-- (reverse [] ++ [3]) ++ [2]) ++ [1]
-- ([] ++ [3]) ++ [2]) ++ [1]

-- walks the list once, building up a left ass ++ expression (but ++ is designed to be right ass)

-- ([] ++ [3]) ++ [2]) ++ [1]
-- ([3] ++ [2]) ++ [1]
-- (3 : [] ++ [2]) ++ [1]
-- (3 : [2]) ++ [1]
-- [3,2] ++ [1]
-- 3 : [2] ++ [1] -- unpacking 3 again!
-- 3 : 2 : [] ++ [1]
-- 3 : 2 : [1]
-- [3,2,1]

one way to think of it is that putting something to the end of list requires rebuilding the whole list
=> expensive!

-}

-- reverse' :: [a] -> [a]

-- (fromList [1,2,4]) [] == [1,2,4]
--    f mempty_1 = mempty_2
--    f (x <>_1 y) = f x <>_2 f y
fromList :: [a] -> ([a] -> [a])
-- fromList xs = \ys -> xs ++ ys -- but ++ again!
fromList [] = id

reverse' :: [a] -> [a]
reverse' xs = go xs []
  where
    go :: [a] -> ([a] -> [a])
    go [] = id
    go (x:xs) = go xs . (x:)


-- toList :: ([a] -> [a]) -> [a]

-- reverse' [1,2,3]
-- go [1,2,3] []
-- ((go [2,3]) . (1:)) []
-- (((go [3]) . (2:)) . (1:)) []
-- ((((go []) . (3:)) . (2:)) . (1:)) []
-- ((((id) . (3:)) . (2:)) . (1:)) []
-- (((3:) . (2:)) . (1:)) []
-- ((3:) . (2:)) ((1:) [])
-- ((3:) . (2:)) [1]
-- (3:) ((2:) [1])
-- (3:) ([2,1])
-- [3,2,1]



-- Monoid actions
  -- ... monoid theme and variation
-- free monoid

-- nick zoo paper