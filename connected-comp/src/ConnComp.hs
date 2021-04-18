module ConnComp
  ( dpConnectedComponents
  , module ConnComp.Internal
  , module Data.ConnComp
  ) where

import           ConnComp.Internal
import           Data.ConnComp
import           Relude


dpConnectedComponents :: IO ()
dpConnectedComponents = runDPConnectedComp


