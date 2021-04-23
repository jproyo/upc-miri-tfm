module Data.ConnComp
  ( Edge(..)
  , ConnectedComponents
  , toEdge
  , toConnectedComp
  , isIncident
  , addToConnectedComp
  , includedIncident
  , Data.ConnComp.null
  , union
  , intersect
  ) where

import           Control.Exception.Base
import           Control.Monad.Catch
import qualified Data.IntSet                                          as S
import           Relude
import           Text.Trifecta
import           Text.Trifecta.Parser                              as P

newtype Edge = Edge (Int, Int)
  deriving (Show, Eq, Ord)

newtype ConnectedComponents = ConnectedComponents IntSet
  deriving newtype (Monoid, Semigroup, Show, Eq)

toEdge :: (MonadIO m, MonadThrow m) => String -> m Edge
toEdge = foldResult (const $ throwM FixIOException) pure . toEdge'

toEdge' :: String -> Text.Trifecta.Result Edge
toEdge' = P.parseString parseEdge mempty

parseInt :: Parser Int
parseInt = fromInteger <$> integer 

parseEdge :: Parser Edge
parseEdge = fmap Edge . (,) <$> (whiteSpace *> parseInt <* whiteSpace) <*> parseInt

toConnectedComp :: Edge -> ConnectedComponents
toConnectedComp (Edge (a, b)) = ConnectedComponents (a `S.insert` S.singleton b)

addToConnectedComp :: Edge -> ConnectedComponents -> ConnectedComponents
addToConnectedComp (Edge (a, b)) (ConnectedComponents set) = ConnectedComponents $ a `S.insert` (b `S.insert` set)

isIncident :: Edge -> Edge -> Bool
isIncident (Edge (a, b)) (Edge (c, d)) = a == c || b == c || a == d || b == d

includedIncident :: Edge -> ConnectedComponents -> Bool
includedIncident (Edge (a, b)) (ConnectedComponents set) = S.member a set || S.member b set

null :: ConnectedComponents -> Bool
null (ConnectedComponents s) = S.null s

intersect :: ConnectedComponents -> ConnectedComponents -> Bool
intersect (ConnectedComponents s1) (ConnectedComponents s2) = not $ S.disjoint s1 s2 

union :: ConnectedComponents -> ConnectedComponents -> ConnectedComponents
union (ConnectedComponents s1) (ConnectedComponents s2) = ConnectedComponents (S.union s1 s2)
