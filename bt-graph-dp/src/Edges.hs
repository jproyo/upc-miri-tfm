-- |
-- Module      : Edges
-- Copyright   : (c) 2021 Juan Pablo Royo Sales
--
-- License     : BSD3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : experimental
-- Portability : GHC
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnboxedTuples #-}
module Edges where

import           Data.IntSet                                       as IS
import           Data.Time.Clock.POSIX
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

data Triplet = Triplet {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
data Pair = Pair {-# UNPACK #-} !Int {-# UNPACK #-} !Int

type UT = [Triplet]

data DW = DW
  { _dwLower :: Pair
  , _dwUpper :: UT
  }

newtype DWTT = DWTT [DW]
  deriving newtype (Semigroup, Monoid)

data BT = BT
  { _btLower :: Triplet
  , _btUpper :: UT
  }

newtype BTTT = BTTT [BT]
  deriving newtype (Semigroup, Monoid)

data BTResult = RBT Q [(Int, Int, Int, Int, Int, Int, Int)]
              | RC  Q Int

data FilterState = Adj W
                 | DoubleWedges DWTT
                 | BiTriangles BTTT


{-# INLINE addWedge #-}
addWedge :: UpperVertex -> IntSet -> IntSet
addWedge = IS.insert

{-# INLINE addDw #-}
addDw :: DW -> DWTT -> DWTT
addDw e (DWTT dw) = DWTT $ e : dw

{-# INLINE hasNotDW #-}
hasNotDW :: DWTT -> Bool
hasNotDW (DWTT x) = R.null x

{-# INLINE hasDW #-}
hasDW :: DWTT -> Bool
hasDW = not . hasNotDW

{-# INLINE t #-}
t :: Integral b => POSIXTime -> IO b
t fct = round . (fct *) <$> getPOSIXTime

{-# INLINE nanoSecs #-}
nanoSecs :: IO Double
nanoSecs = (/ 1000000) . fromInteger <$> t 1000000000

{-# INLINE microSecs #-}
microSecs :: IO Double
microSecs = (/ 1000) . fromInteger <$> t 1000000

{-# INLINE milliSecs #-}
milliSecs :: IO Double
milliSecs = fromInteger <$> t 1000

{-# INLINE showFullPrecision #-}
showFullPrecision :: Double -> String
showFullPrecision = flip (showFFloat Nothing) ""

{-# INLINE printHeader #-}
printHeader :: IO ()
printHeader = putBSLn "test,command,answer,number,time"

{-# INLINE printCC #-}
printCC :: BTResult -> Int -> IO ()
printCC (RBT (Q q startTime name) bt) c = do
  now <- nanoSecs
  forM_ bt $ \path -> putLBSLn $ encodeUtf8 $ intercalate
    ","
    [toString name, R.show q, R.show path, R.show c, showFullPrecision (now - startTime)]
printCC _ _ = putLBSLn "No Result can be shown for this command"

{-# INLINE modifyWState #-}
modifyWState :: FilterState -> UpperVertex -> FilterState
modifyWState (Adj w@(W _ ws)) u = Adj $ w { _wWedges = addWedge u ws }
modifyWState s                _ = s

{-# INLINE modifyDWState #-}
modifyDWState :: FilterState -> DW -> FilterState
modifyDWState (DoubleWedges d) dw = DoubleWedges $ addDw dw d
modifyDWState s                _  = s

{-# INLINE modifyBTState #-}
modifyBTState :: FilterState -> BT -> FilterState
modifyBTState (BiTriangles b) bt = BiTriangles $ addBt bt b
modifyBTState s               _  = s

{-# INLINE toBTPath #-}
toBTPath :: BT -> [(Int, Int, Int, Int, Int, Int, Int)]
toBTPath BT {..} =
  let (Triplet l_l l_m l_u) = _btLower 
   in R.map (\(Triplet u_1 u_2 u_3) -> (l_l, u_1, l_m, u_3, l_u, u_2, l_l)) _btUpper


{-# INLINE isInTriple #-}
isInTriple :: Triplet -> Int -> Bool
isInTriple (Triplet a b c) vertex = a == vertex || b == vertex || c == vertex

{-# INLINE isInTriple' #-}
isInTriple' :: Triplet -> Int -> Bool
isInTriple' (Triplet a b c) vertex = a == vertex || b == vertex || c == vertex

{-# INLINE hasVertex #-}
hasVertex :: BT -> Int -> Bool
hasVertex BT {..} vertex = isInTriple' _btLower vertex || any (`isInTriple` vertex) _btUpper

{-# INLINE hasEdge #-}
hasEdge :: BT -> Edge -> Bool
hasEdge BT {..} edge = any (isInEdge edge _btLower) _btUpper

{-# INLINE isInEdge #-}
isInEdge :: Edge -> Triplet -> Triplet -> Bool
isInEdge (u, l) (Triplet l1 l2 l3) (Triplet u1 u2 u3) =
  (u == u1 && l1 == l)
    || (u == u2 && l1 == l)
    || (u == u1 && l2 == l)
    || (u == u3 && l2 == l)
    || (u == u2 && l3 == l)
    || (u == u3 && l3 == l)


{-# INLINE addBt #-}
addBt :: BT -> BTTT -> BTTT
addBt bt (BTTT bts) = BTTT (bt : bts)

{-# INLINE containsVertex #-}
containsVertex :: [Int] -> BTTT -> Bool
containsVertex vertices (BTTT bts) = R.any (\a -> R.any (hasVertex a) vertices) bts

{-# INLINE containsEdges #-}
containsEdges :: [Edge] -> BTTT -> Bool
containsEdges edges (BTTT bts) = R.any (\a -> R.any (hasEdge a) edges) bts

{-# INLINE hasNotBT #-}
hasNotBT :: BTTT -> Bool
hasNotBT (BTTT bts) = R.null bts

nonEdge :: Edge
nonEdge = (-1, -1)

{-# INLINE toEdge #-}
toEdge :: String -> Edge
toEdge = foldResult (const nonEdge) identity . toEdge'

{-# INLINE toEdge' #-}
toEdge' :: String -> Text.Trifecta.Result Edge
toEdge' = P.parseString parseEdge mempty

{-# INLINE parseInt #-}
parseInt :: Parser Int
parseInt = fromInteger <$> integer

{-# INLINE parseEdge #-}
parseEdge :: Parser Edge
parseEdge = (,) <$> (whiteSpace *> parseInt <* whiteSpace) <*> parseInt <* whiteSpace

{-# INLINE toCommand #-}
toCommand :: String -> Command
toCommand = foldResult (const NoCommand) identity . toCommand'

{-# INLINE toCommand' #-}
toCommand' :: String -> Text.Trifecta.Result Command
toCommand' = P.parseString parseCommand mempty

{-# INLINE parseCommand #-}
parseCommand :: Parser Command
parseCommand = byVertex <|> byEdge <|> countQ <|> allQ <|> endQ

{-# INLINE byVertex #-}
byVertex :: Parser Command
byVertex = ByVertex <$> (string "by-vertex" *> (whiteSpace *> many parseInt))

{-# INLINE parseEdgeWithComma #-}
parseEdgeWithComma :: Parser Edge
parseEdgeWithComma =
  (,)
    <$> (whiteSpace *> string "(" *> parseInt <* whiteSpace <* string "," <* whiteSpace)
    <*> parseInt
    <*  string ")"
    <*  whiteSpace

{-# INLINE byEdge #-}
byEdge :: Parser Command
byEdge = ByEdge <$> (string "by-edge" *> (whiteSpace *> many parseEdgeWithComma))

{-# INLINE countQ #-}
countQ :: Parser Command
countQ = string "count" $> Count

{-# INLINE allQ #-}
allQ :: Parser Command
allQ = string "all" $> AllBT

{-# INLINE endQ #-}
endQ :: Parser Command
endQ = string "end" $> End

{-# INLINE commandsText #-}
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
