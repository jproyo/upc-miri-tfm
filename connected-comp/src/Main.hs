module Main where

import           Control.Concurrent
import           Control.Monad.Catch
import           Relude                                            as R
import           Streamly
import qualified Streamly.FileSystem.Handle                        as FH
import qualified Streamly.Internal.Data.Fold                       as FL
import           Streamly.Internal.Data.Stream.StreamK             as SD
import qualified Streamly.Internal.Data.Unicode.Stream             as US
import qualified Streamly.Internal.FileSystem.File                 as F
import qualified Streamly.Internal.Memory.Array                    as SA
import           Streamly.Prelude                                  as S
import           Text.Trifecta
import           Text.Trifecta.Parser                              as P

newtype Edge a = Edge (a, a)
  deriving (Functor, Show, Eq, Generic)

newtype Edges a = Edges [Edge a]
  deriving (Functor, Show, Eq, Generic)

newtype ConnectedComponents a = ConnectedComponents [a]
  deriving newtype (Functor, Applicative, Show, Eq)

newtype Result a = Result (ConnectedComponents a)
  deriving newtype (Functor, Applicative, Show, Eq)

main :: IO ()
main = setup >> prog

setup :: IO ()
setup = hSetBuffering stdout LineBuffering

prog :: IO ()
prog = S.drain $ aheadly runDPConnectedComp

runDPConnectedComp :: (IsStream t, MonadIO (t m), MonadFail m, MonadAsync m, MonadCatch m) => t m ()
runDPConnectedComp = input |& generator |& output

input :: (IsStream t, MonadIO (t m), MonadFail m, MonadAsync m, MonadCatch m) => t m (Edge Integer)
input = do
  file <- maybe (liftIO $ fail "Error no parameter found") return . R.viaNonEmpty R.head =<< liftIO getArgs
  F.withFile file ReadMode $ \h ->
    S.unfold FH.readChunksWithBufferOf (256 * 1024, h)
      & S.concatMap SA.toStream
      & US.decodeUtf8
      & US.lines FL.toList
      & parseEdges

parseEdges :: (IsStream t, MonadFail m, MonadAsync m) => t m String -> t m (Edge Integer)
parseEdges = S.mapM (foldResult (fail . show) pure) . S.map (P.parseString parseEdge mempty)

parseEdge :: Parser (Edge Integer)
parseEdge = fmap Edge . (,) <$> (integer <* whiteSpace) <*> integer

generator :: (IsStream t, MonadAsync m) => t m (Edge Integer) -> t m (Edge Integer)
generator = SD.foldrS (\a@(Edge (v1, _)) b -> if even v1 then S.yield a <> b |& newFilter a else S.yield a <> b) nil

newFilter :: (IsStream t, MonadAsync m) => Edge Integer -> t m (Edge Integer) -> t m (Edge Integer)
newFilter = S.mapM . filtering

filtering :: (MonadAsync m) => Edge Integer -> Edge Integer -> m (Edge Integer)
filtering y x@(Edge (v1, _)) = do
  if v1 == 2 then liftIO $ threadDelay 1000000 else pure ()
  return (if x == y then fmap (+ 20) x else x)

output :: (IsStream t, MonadAsync m) => t m (Edge Integer) -> t m ()
output = S.mapM print

