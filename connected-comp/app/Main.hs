module Main where

import           ConnComp
import           Relude

main :: IO ()
main = setup >> dpConnectedComponents

setup :: IO ()
setup = hSetBuffering stdout LineBuffering
