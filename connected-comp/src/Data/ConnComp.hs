module Data.ConnComp
  ( Edge
  , ConnectedComponents
  , toEdge
  , toConnectedComp
  , isIncident
  , addToConnectedComp
  ) where

import qualified Data.Set                                          as S
import           Relude
import           Text.Trifecta
import           Text.Trifecta.Parser                              as P

newtype Edge a = Edge (a, a)
  deriving (Functor, Show, Eq)

newtype ConnectedComponents a = ConnectedComponents (Set a)
  deriving newtype (Monoid, Semigroup, Show, Eq)

toEdge :: (MonadIO m, MonadFail m) => String -> m (Edge Integer)
toEdge = foldResult (fail . show) pure . toEdge'

toEdge' :: String -> Text.Trifecta.Result (Edge Integer)
toEdge' = P.parseString parseEdge mempty

parseEdge :: Parser (Edge Integer)
parseEdge = fmap Edge . (,) <$> (integer <* whiteSpace) <*> integer

toConnectedComp :: Ord a => Edge a -> ConnectedComponents a
toConnectedComp (Edge (a, b)) = ConnectedComponents (a `S.insert` S.singleton b)

addToConnectedComp :: Ord a => Edge a -> ConnectedComponents a -> ConnectedComponents a
addToConnectedComp (Edge (a, b)) (ConnectedComponents set) = ConnectedComponents $ a `S.insert` (b `S.insert` set)

isIncident :: Eq a => Edge a -> Edge a -> Bool
isIncident (Edge (a, b)) (Edge (c, d)) = a == c || b == c || a == d || b == d

