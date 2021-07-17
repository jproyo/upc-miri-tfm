-- |
-- Module      : Edges
-- Copyright   : (c) 2021 Juan Pablo Royo Sales
--
-- License     : BSD3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : experimental
-- Portability : GHC

module Edges where

import           Data.IntSet                                       as IS
import           Data.Set                                          as S
import           Relude                                            as R
import           Text.Trifecta
import           Text.Trifecta.Parser                              as P

type LowerVertex = Int
type UpperVertex = Int
type Edge = (UpperVertex, LowerVertex)

data W = W
  { _wLowerVertex :: LowerVertex
  , _wWedges      :: IntSet
  }
  deriving Show

addWedge :: IntSet -> UpperVertex -> IntSet
addWedge = flip IS.insert

type UT = Set (UpperVertex, UpperVertex, UpperVertex)

data DW = DW
  { _dwLower :: (LowerVertex, LowerVertex)
  , _dwUpper :: UT
  }
  deriving Show

newtype DWTT = DWTT [DW]
  deriving newtype Show

addDw :: DW -> DWTT -> DWTT
addDw e (DWTT dw) = DWTT $ e : dw

hasNotDW :: DWTT -> Bool
hasNotDW (DWTT x) = R.null x

data BT = BT
  { _btLower :: (LowerVertex, LowerVertex, LowerVertex)
  , _btUpper :: UT
  }
  deriving Show

newtype BTTT = BTTT [BT]
  deriving newtype Show

addBt :: BT -> BTTT -> BTTT
addBt e (BTTT bt) = BTTT $ e : bt

hasNotBT :: BTTT -> Bool
hasNotBT (BTTT x) = R.null x

nonEdge :: Edge
nonEdge = (-1, -1)

toEdge :: String -> Edge
toEdge = foldResult (const nonEdge) identity . toEdge'

toEdge' :: String -> Text.Trifecta.Result Edge
toEdge' = P.parseString parseEdge mempty

parseInt :: Parser Int
parseInt = fromInteger <$> integer

parseEdge :: Parser Edge
parseEdge = (,) <$> (whiteSpace *> parseInt <* whiteSpace) <*> parseInt

