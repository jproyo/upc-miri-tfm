module Data.ConnComp
  ( Edge(..)
  , ConnectedComponents
  , toEdge
  , toConnectedComp
  , isIncident
  , addToConnectedComp
  , includedIncident
  , Data.ConnComp.null
  , StreamState(..)
  ) where

import           Control.Exception.Base
import           Control.Monad.Catch
import qualified Data.Set                                          as S
import           Relude
import           Text.Trifecta
import           Text.Trifecta.Parser                              as P

newtype Edge a = Edge (a, a)
  deriving (Functor, Show, Eq, Ord)

newtype ConnectedComponents a = ConnectedComponents (Set a)
  deriving newtype (Monoid, Semigroup, Show, Eq)


data StreamState a = ByPass (Edge a)
                   | Computed (ConnectedComponents a)
                   deriving Show

toEdge :: (MonadIO m, MonadThrow m) => String -> m [Edge Integer]
toEdge = foldResult (const $ throwM FixIOException) pure . toEdge'

toEdge' :: String -> Text.Trifecta.Result [Edge Integer]
toEdge' = P.parseString (many parseEdge) mempty

parseEdge :: Parser (Edge Integer)
parseEdge = fmap Edge . (,) <$> (integer <* whiteSpace) <*> integer

toConnectedComp :: Ord a => Edge a -> ConnectedComponents a
toConnectedComp (Edge (a, b)) = ConnectedComponents (a `S.insert` S.singleton b)

addToConnectedComp :: Ord a => Edge a -> ConnectedComponents a -> ConnectedComponents a
addToConnectedComp (Edge (a, b)) (ConnectedComponents set) = ConnectedComponents $ a `S.insert` (b `S.insert` set)

isIncident :: Eq a => Edge a -> Edge a -> Bool
isIncident (Edge (a, b)) (Edge (c, d)) = a == c || b == c || a == d || b == d

includedIncident :: Eq a => Edge a -> ConnectedComponents a -> Bool
includedIncident (Edge (a, b)) (ConnectedComponents set) = getAny $ foldMap (\e -> Any (a == e || b == e)) set

null :: ConnectedComponents a -> Bool
null (ConnectedComponents s) = S.null s