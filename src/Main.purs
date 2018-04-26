module Main where

import Control.Monad.Eff.Console
import Prelude
import Ch2


main = do
  log "Chapter 2 exercises"
  logShow (diagonal 3.0 4.0)
  logShow (diagonal 5.0 4.0)
