module Main where

import ConnCompDPF ( program )
import           Relude as R

main :: IO ()
main = do 
    file <- maybe (fail "Error no parameter found") return . R.viaNonEmpty R.head =<< getArgs
    program file

