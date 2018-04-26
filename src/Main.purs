module Main where

import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Prelude (Unit)
import Ch2


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  ch2
