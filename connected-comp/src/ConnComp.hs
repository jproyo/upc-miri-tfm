module ConnComp
  ( dpConnectedComponents
  ) where

import           ConnComp.Internal
import           Relude
import           Streamly
import           Streamly.Prelude                                  as S


dpConnectedComponents :: IO ()
dpConnectedComponents = S.drain $ aheadly runDPConnectedComp


