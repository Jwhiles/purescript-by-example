module Ch4 where

import Prelude

import Data.Array (length, null, filter, (..))
import Data.Array.Partial (tail, head)
import Data.Int (even)
import Data.Foldable (product)
import Partial.Unsafe (unsafePartial)
import Control.MonadZero (guard)

-- recursion ex

recursiveEven :: Int -> Boolean
recursiveEven 0 = true
recursiveEven 1 = false
recursiveEven n = if n > 0 then recursiveEven $ n - 2 else false

evenInts :: Array Int -> Int
evenInts arr =
  if null arr
    then 0
    else let rest = evenInts (unsafePartial tail arr)
          in if (even $ unsafePartial head arr) then 1 + rest else 0 + rest

-- maps ex

squareArray :: Array Int -> Array Int
squareArray arr = (\x -> x * x) <$> arr
squareArray' :: Array Int -> Array Int
squareArray' = map (\x -> x * x)

filterNegatives :: Array Int -> Array Int
filterNegatives = filter (\x -> x > 0) 
-- I get an error about needing to import when I run this, but I can't figure
-- out where negate should be imported from. oh no

infix 8 filter as <$?> 

filterNegatives' :: Array Int -> Array Int
filterNegatives' a = (\x -> x > 0) <$?> a

-- comprehensions

factors :: Int -> Array (Array Int)
factors n = (\xs -> product xs == n) <$?> do
  i <- 1 .. n
  j <- i .. n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime n = (length $ factors n) == 1

cartesianProd :: Array Int -> Array Int -> Array (Array Int)
cartesianProd xs ys = do
  x <- xs
  y <- ys
  pure [x, y]

pythagTriple :: Int -> Array (Array Int)
pythagTriple n = do
  x <- 1 .. n
  y <- 1 .. n
  z <- 1 .. n
  guard $ (x * x) + (y * y) == (z * z)
  pure [x, y, z]
