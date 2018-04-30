module Ch4 where

import Prelude

import Data.Array (null)
import Data.Array.Partial (tail, head)
import Data.Int (even)
import Partial.Unsafe (unsafePartial)

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
