-- |
-- Module      : Edges
-- Copyright   : (c) 2021 Juan Pablo Royo Sales
--
-- License     : BSD3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : experimental
-- Portability : GHC
{-# LANGUAGE QuasiQuotes #-}
module Edges where

import           Data.IntSet                                       as IS
import           Data.Set                                          as S
import           GHC.Show                                                                                             ( show
                                                                                                                      )
import           Relude                                            as R
import           Text.RawString.QQ
import           Text.Trifecta
import           Text.Trifecta.Parser                              as P

type LowerVertex = Int
type UpperVertex = Int
type Edge = (UpperVertex, LowerVertex)

-- | Command query
data Q = ByVertex [Int]
       | ByEdge [Edge]
       | Count
       | AllBT
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

hasDW :: DWTT -> Bool
hasDW = not . hasNotDW

data BTResult = RBT BT Double
              | RC  Int Double

instance Show BTResult where
  show (RBT bt _) = R.show bt
  show (RC  c  _) = R.show c

data BT = BT
  { _btLower :: (LowerVertex, LowerVertex, LowerVertex)
  , _btUpper :: UT
  }
  deriving Show

data BTTT = BTTT
  { _btttKeys  :: IntSet
  , _btttEdges :: Set Edge
  , _btttBts   :: [BT]
  }
  deriving Show

instance Monoid BTTT where
  mempty = BTTT mempty mempty mempty

instance Semigroup BTTT where
  bts1 <> bts2 = BTTT { _btttKeys  = _btttKeys bts1 <> _btttKeys bts2
                      , _btttEdges = _btttEdges bts1 <> _btttEdges bts2
                      , _btttBts   = _btttBts bts1 <> _btttBts bts2
                      }

addBt :: BT -> BTTT -> BTTT
addBt bt@BT {..} bts =
  let
    newBtKeys = S.foldl (\bs (a, b, c) -> a `IS.insert` (b `IS.insert` (c `IS.insert` bs)))
                        IS.empty
                        (S.singleton _btLower <> _btUpper)
    (l_1, l_2, l_3) = _btLower
    combineEdges bs (u_1, u_2, u_3) =
      (u_1, l_1)
        `S.insert` (          (u_2, l_1)
                   `S.insert` (          (u_1, l_2)
                              `S.insert` ((u_3, l_2) `S.insert` ((u_2, l_3) `S.insert` ((u_3, l_3) `S.insert` bs)))
                              )
                   )
    newBtEdges = S.foldl combineEdges S.empty _btUpper
  in
    bts { _btttKeys  = _btttKeys bts <> newBtKeys
        , _btttEdges = _btttEdges bts <> newBtEdges
        , _btttBts   = bt : _btttBts bts
        }

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
parseCommand = byVertex <|> byEdge <|> countQ <|> allQ <|> endQ

byVertex :: Parser Q
byVertex = ByVertex <$> (string "by-vertex" *> (whiteSpace *> many parseInt))

parseEdgeWithComma :: Parser Edge
parseEdgeWithComma =
  (,)
    <$> (whiteSpace *> string "(" *> parseInt <* whiteSpace <* string "," <* whiteSpace)
    <*> parseInt
    <*  string ")"
    <*  whiteSpace

byEdge :: Parser Q
byEdge = ByEdge <$> (string "by-edge" *> (whiteSpace *> many parseEdgeWithComma))

countQ :: Parser Q
countQ = string "count" $> Count

allQ :: Parser Q
allQ = string "all" $> AllBT

endQ :: Parser Q
endQ = string "end" $> End

commandsText :: Text
commandsText = [r|
by-vertex LIST_VERTEX_SPLIT_BY_SPACE       Return all Bitriangles that contains any of the vertices in the list
  
  Example: by-vertex 1 4 87

by-edge LIST_EDGES_SPLIT_BY_SPACE          Return all Bitriangles that contains any of the edges in the list
  
  Example: by-edge (1,101) (3,104)

all                                        Enumerate all the Bitriangles. WARNING: This should be use for testing purpose only

count                                      Count all the Bitriangles

end                                        Finish command input and close program
 |]