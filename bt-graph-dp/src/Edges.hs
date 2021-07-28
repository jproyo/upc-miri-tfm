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
import           Data.Time.Clock.POSIX
import           GHC.Show                                                                                             ( show
                                                                                                                      )
import           Numeric
import           Relude                                            as R
import           Text.RawString.QQ
import           Text.Trifecta
import           Text.Trifecta.Parser                              as P

data Conf = Conf
  { _edgeFile       :: FilePath
  , _commandFile    :: FilePath
  , _experimentName :: Text
  }

type LowerVertex = Int
type UpperVertex = Int
type Edge = (UpperVertex, LowerVertex)

-- | Command query
data Q = Q
  { _command   :: Command
  , _startTime :: Double
  , _expName   :: Text
  }
  deriving Show

data Command = ByVertex [Int]
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

t :: Integral b => POSIXTime -> IO b
t fct = round . (fct *) <$> getPOSIXTime

nanoSecs :: IO Double
nanoSecs = (/ 1000000) . fromInteger <$> t 1000000000

microSecs :: IO Double
microSecs = (/ 1000) . fromInteger <$> t 1000000

milliSecs :: IO Double
milliSecs = fromInteger <$> t 1000

showFullPrecision :: Double -> String
showFullPrecision = flip (showFFloat Nothing) ""

printHeader :: IO ()
printHeader = putBSLn "test,command,answer,number,time"

printCC :: BTResult -> Int -> IO ()
printCC (RBT (Q q startTime name) bt) c = do
  now <- nanoSecs
  forM_ (toBTPath bt) $ \path -> putLBSLn $ encodeUtf8 $ intercalate
    ","
    [toString name, R.show q, R.show path, R.show c, showFullPrecision (now - startTime)]
printCC x _ = putLBSLn $ R.show x


data BTResult = RBT Q BT
              | RC  Q Int

instance Show BTResult where
  show (RBT _ bt) = R.show bt
  show (RC  _ c ) = R.show c

data BT = BT
  { _btLower :: (LowerVertex, LowerVertex, LowerVertex)
  , _btUpper :: UT
  }
  deriving Show

toBTPath :: BT -> [(Int, Int, Int, Int, Int, Int, Int)]
toBTPath BT {..} =
  let (l_l, l_m, l_u) = _btLower in [ (l_l, u_1, l_m, u_3, l_u, u_2, l_l) | (u_1, u_2, u_3) <- S.toList _btUpper ]

isInTriple :: (Int, Int, Int) -> Int -> Bool
isInTriple (a, b, c) vertex = a == vertex || b == vertex || c == vertex

hasVertex :: BT -> Int -> Bool
hasVertex BT {..} vertex = isInTriple _btLower vertex || any (`isInTriple` vertex) (S.toList _btUpper)

hasEdge :: BT -> Edge -> Bool
hasEdge BT {..} edge = any (isInEdge edge _btLower) (S.toList _btUpper)

isInEdge :: Edge -> (LowerVertex, LowerVertex, LowerVertex) -> (UpperVertex, UpperVertex, UpperVertex) -> Bool
isInEdge (u, l) (l1, l2, l3) (u1, u2, u3) =
  (u == u1 && l1 == l)
    || (u == u2 && l1 == l)
    || (u == u1 && l2 == l)
    || (u == u3 && l2 == l)
    || (u == u2 && l3 == l)
    || (u == u3 && l3 == l)

newtype BTTT = BTTT
  {  _btttBts   :: [BT]
  }
  deriving newtype (Semigroup, Monoid, Show)

addBt :: BT -> BTTT -> BTTT
addBt bt bts = bts { _btttBts = bt : _btttBts bts }

containsVertex :: [Int] -> BTTT -> Bool
containsVertex vertices BTTT {..} = any (\a -> any (hasVertex a) vertices) _btttBts

containsEdges :: [Edge] -> BTTT -> Bool
containsEdges edges BTTT {..} = any (\a -> any (hasEdge a) edges) _btttBts

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

toCommand :: String -> Command
toCommand = foldResult (const NoCommand) identity . toCommand'

toCommand' :: String -> Text.Trifecta.Result Command
toCommand' = P.parseString parseCommand mempty

parseCommand :: Parser Command
parseCommand = byVertex <|> byEdge <|> countQ <|> allQ <|> endQ

byVertex :: Parser Command
byVertex = ByVertex <$> (string "by-vertex" *> (whiteSpace *> many parseInt))

parseEdgeWithComma :: Parser Edge
parseEdgeWithComma =
  (,)
    <$> (whiteSpace *> string "(" *> parseInt <* whiteSpace <* string "," <* whiteSpace)
    <*> parseInt
    <*  string ")"
    <*  whiteSpace

byEdge :: Parser Command
byEdge = ByEdge <$> (string "by-edge" *> (whiteSpace *> many parseEdgeWithComma))

countQ :: Parser Command
countQ = string "count" $> Count

allQ :: Parser Command
allQ = string "all" $> AllBT

endQ :: Parser Command
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
