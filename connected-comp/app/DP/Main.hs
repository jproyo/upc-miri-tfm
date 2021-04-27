module Main where

import ConnComp ( runDPConnectedComp )
import           Relude as R

main :: IO ()
main = do 
    file <- maybe (fail "Error no parameter found") return . R.viaNonEmpty R.head =<< getArgs
    runDPConnectedComp file

