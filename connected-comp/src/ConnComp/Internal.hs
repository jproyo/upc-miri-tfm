module ConnComp.Internal
  ( runDPConnectedComp
  ) where

import           Control.Concurrent
import           Control.Monad.Catch
import           Data.ConnComp                 as DC
import           Relude                        as R
import           Streamly
import qualified Streamly.FileSystem.Handle    as FH
import qualified Streamly.Internal.Data.Array.Foreign.Type
                                               as ST
import qualified Streamly.Internal.Data.Fold   as FL
import qualified Streamly.Internal.Data.Stream.IsStream.Expand
                                               as SE
import           Streamly.Internal.Data.Stream.StreamK
                                               as SD
import qualified Streamly.Internal.FileSystem.File
                                               as F
import qualified Streamly.Internal.Unicode.Stream
                                               as US
import           Streamly.Prelude              as S


runDPConnectedComp :: (IsStream t, MonadIO (t m), MonadAsync m, MonadCatch m)
                   => t m ()
runDPConnectedComp = input |& generator |& output

bucket :: Edge Integer -> (Edge Integer, ConnectedComponents Integer)
bucket = identity &&& toConnectedComp

input :: (IsStream t, MonadIO (t m), MonadThrow m, MonadAsync m, MonadCatch m)
      => t m (Edge Integer)
input = do
  file <-
    maybe (liftIO $ fail "Error no parameter found") return
    .   R.viaNonEmpty R.head
    =<< liftIO getArgs
  F.withFile file ReadMode $ \h ->
    S.unfold FH.readChunksWithBufferOf (256 * 1024, h)
      & SD.concatMap ST.toStream
      & US.decodeUtf8
      & US.lines FL.toList
      & parseEdges

parseEdges :: (IsStream t, MonadAsync m) => t m String -> t m (Edge Integer)
parseEdges = S.mapM toEdge



generator :: (IsStream t, MonadAsync m) => t m (Edge Integer) -> t m (ConnectedComponents Integer)
generator = SD.foldrS generateFilter nil

generateFilter :: (IsStream t, MonadAsync m) => Edge Integer -> t m (Edge Integer) -> t m (ConnectedComponents Integer)
generateFilter headElem prevStream = S.yield headElem <> prevStream |& generator' headElem

-- Check https://github.com/composewell/streamly/blob/9b98f41c6b47ddb641503ab7485ae8e41f70979e/src/Streamly/Internal/Data/Stream/IsStream/Expand.hs#L472


generator' :: (IsStream t, MonadAsync m)
          => t m (Edge Integer)
          -> t m (Either (Edge Integer) (ConnectedComponents Integer))
generator' =
  SE.iterateSmapMWith ahead
                      newFilter
                      (pure (mempty :: ConnectedComponents Integer))
    . S.map Left

newFilter' :: (IsStream t, MonadAsync m)
          => ConnectedComponents Integer
          -> Either (Edge Integer) (ConnectedComponents Integer)
          -> m
               ( ConnectedComponents Integer
               , t
                   m
                   ( Either
                       (Edge Integer)
                       (ConnectedComponents Integer)
                   )
               )
newFilter' cc st@(Left edge)
  | edge `includedIncident` cc || DC.null cc
  = let newCC = edge `addToConnectedComp` cc
    in  return (newCC, S.yield st)
  | otherwise
  = return (cc, S.yield st)
newFilter cc st = return (cc, S.yield st)




-- generator :: (IsStream t, MonadAsync m) => t m (Edge Integer) -> t m (Edge Integer)
-- generator = SD.foldrS generateFilter nil

-- & S.fold (FL.tee FL.length FL.length))

-- generateFilter :: (IsStream t, MonadAsync m) => Edge Integer -> t m (Edge Integer) -> t m (Edge Integer)
-- generateFilter headElem prevStream = S.yield headElem <> prevStream |& newFilter headElem

-- newFilter :: (IsStream t, MonadAsync m) => Edge Integer -> t m (Edge Integer) -> t m (Edge Integer)
-- newFilter c = SD.foldrS (execState filtering' c) (S.yield c <> nil)

-- filtering' :: (IsStream t, MonadAsync m, MonadState (ConnectedComponents Integer) m)
--            => Edge Integer
--            -> t m (Edge Integer)
--            -> t m (Edge Integer)
-- filtering' a b = get >>= \c' -> if a `includedIncident` c' then put (addToConnectedComp a c') >> S.yield a <> b else b

traceWithThread :: (IsStream t, MonadIO m, MonadAsync m, Show b, Show a)
                => a
                -> t m b
                -> t m b
traceWithThread c = S.mapM
  (\a ->
    liftIO myThreadId
      >>= liftIO
      .   R.putStrLn
      .   mappend (show a)
      .   mappend (" - Filter " <> show c <> " - ")
      .   show
      >>  return a
  )


output :: (IsStream t, MonadAsync m)
       => t m (Either (Edge Integer) (ConnectedComponents Integer))
       -> t m ()
output = S.mapM print

