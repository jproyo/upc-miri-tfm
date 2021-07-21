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

-- | Command query
data Q = ByVertex [Int]
       | Count
       | NoCommand
       | End
  deriving (Show, Read)

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
  deriving newtype (Show, Semigroup, Monoid)

addDw :: DW -> DWTT -> DWTT
addDw e (DWTT dw) = DWTT $ e : dw

hasNotDW :: DWTT -> Bool
hasNotDW (DWTT x) = R.null x

data BT = BT
  { _btLower :: (LowerVertex, LowerVertex, LowerVertex)
  , _btUpper :: UT
  }
  deriving Show

data BTTT = BTTT
  { _btttKeys :: IntSet
  , _btttBts  :: [BT]
  }

instance Monoid BTTT where
  mempty = BTTT mempty mempty

instance Semigroup BTTT where
  bts1 <> bts2 = BTTT { _btttKeys = _btttKeys bts1 <> _btttKeys bts2, _btttBts = _btttBts bts1 <> _btttBts bts2 }

addBt :: BT -> BTTT -> BTTT
addBt bt@BT {..} bts =
  let newBtKeys = S.foldl (\bs (a, b, c) -> a `IS.insert` (b `IS.insert` (c `IS.insert` bs)))
                          IS.empty
                          (S.singleton _btLower <> _btUpper)
  in  bts { _btttKeys = _btttKeys bts <> newBtKeys, _btttBts = bt : _btttBts bts }

hasNotBT :: BTTT -> Bool
hasNotBT BTTT {..} = R.null _btttBts

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

toCommand :: String -> Q
toCommand = foldResult (const NoCommand) identity . toCommand'

toCommand' :: String -> Text.Trifecta.Result Q
toCommand' = P.parseString parseCommand mempty

parseCommand :: Parser Q
parseCommand = byVertex <|> countQ <|> endQ

byVertex :: Parser Q
byVertex = ByVertex <$> (string "vertices in" *> (whiteSpace *> many parseInt))

countQ :: Parser Q
countQ = string "count" $> Count

endQ :: Parser Q
endQ = string "end" $> End
