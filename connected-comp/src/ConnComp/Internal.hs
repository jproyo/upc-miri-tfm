module ConnComp.Internal
  ( runDPConnectedComp
  ) where

import           Control.Concurrent
import           Control.Monad.Catch
import           Data.ConnComp
import           Relude                                            as R
import           Streamly
import qualified Streamly.FileSystem.Handle                        as FH
import qualified Streamly.Internal.Data.Fold                       as FL
import           Streamly.Internal.Data.Stream.StreamK             as SD
import qualified Streamly.Internal.Data.Unicode.Stream             as US
import qualified Streamly.Internal.FileSystem.File                 as F
import qualified Streamly.Internal.Memory.Array                    as SA
import           Streamly.Prelude                                  as S


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
parseEdges = S.mapM toEdge

generator :: (IsStream t, MonadAsync m) => t m (Edge Integer) -> t m Edge Integer
generator = SD.foldrS generateFilter nil . S.map (identity &&& toConnectedComp)

generateFilter :: (IsStream t, MonadAsync m)
               => (Edge Integer, ConnectedComponents Integer)
               -> t m (Edge Integer, ConnectedComponents Integer)
               -> t m (Edge Integer, ConnectedComponents Integer)
generateFilter headElem prevStream = S.yield headElem <> prevStream |& newFilter headElem

newFilter :: (IsStream t, MonadAsync m)
          => (Edge Integer, ConnectedComponents Integer)
          -> t m (Edge Integer, ConnectedComponents Integer)
          -> t m (Edge Integer, ConnectedComponents Integer)
newFilter c = S.map (filtering c) . S.mapM
  (\a ->
    liftIO myThreadId
      >>= liftIO
      .   R.putStrLn
      .   mappend (show a)
      .   mappend (" - Filter " <> show c <> " - ")
      .   show
      >>  return a
  )

filtering :: (Edge Integer, ConnectedComponents Integer)
          -> (Edge Integer, ConnectedComponents Integer)
          -> (Edge Integer, ConnectedComponents Integer)
filtering (edge, st) c@(newEdge, _) | edge `isIncident` newEdge = (newEdge, addToConnectedComp newEdge st)
                                    | otherwise                 = c


output :: (IsStream t, MonadAsync m) => t m (Edge Integer, ConnectedComponents Integer) -> t m ()
output = S.mapM (print . snd)

