module Main
  ( main
  ) where

import           ConnComp (calculate)
import           Relude  as R



main :: IO ()
main = do
  file <- maybe (fail "Error no parameter found") return . R.viaNonEmpty R.head =<< getArgs
  calculate file
