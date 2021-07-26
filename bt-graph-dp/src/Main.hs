module Main where

import           BTriangle
import           Control.Monad
import           Relude                                            as R
import           System.Environment

main :: IO ()
main = do
  file <- maybe (fail "Error no parameter found") return . R.viaNonEmpty R.head =<< getArgs
  program file


