module Ch2 where

import Control.Monad.Eff.Console
import Math (sqrt, pi)
import Prelude

diagonal w h = sqrt (w * w + h * h)

circleArea r = pi * r * r

ch2 = do
  log "Chapter 2 exercises"
  logShow (diagonal 3.0 4.0)
  logShow (circleArea 3.0)
