module ConnComp.Internal
  ( runDPConnectedComp
  ) where

--import           Control.Concurrent
import           Control.Monad.Catch
import           Data.ConnComp                                     as DC
import           Relude                                            as R
import           Streamly
import qualified Streamly.FileSystem.Handle                        as FH
import qualified Streamly.Internal.Data.Array.Foreign.Type         as ST
import qualified Streamly.Internal.Data.Fold                       as FL
import qualified Streamly.Internal.Data.Stream.IsStream.Expand     as SE
import           Streamly.Internal.Data.Stream.StreamK             as SD
import qualified Streamly.Internal.Data.Unfold                     as FU
import qualified Streamly.Internal.FileSystem.File                 as F
import qualified Streamly.Internal.Unicode.Stream                  as US
import           Streamly.Prelude                                  as S


runDPConnectedComp :: (IsStream t, MonadIO (t m), MonadAsync m, MonadCatch m) =>  t m ()
runDPConnectedComp =  input |& generator'' |& S.mapM print -- output

input :: (IsStream t, MonadIO (t m), MonadThrow m, MonadAsync m, MonadCatch m) => t m (Edge Integer)
input = do
  file <- maybe (liftIO $ fail "Error no parameter found") return . R.viaNonEmpty R.head =<< liftIO getArgs
  F.withFile file ReadMode $ \h ->
    S.unfold FH.readChunksWithBufferOf (256 * 1024, h)
      & SD.concatMap ST.toStream
      & US.decodeUtf8
      & US.lines FL.toList
      & parseEdges

parseEdges :: (IsStream t, MonadAsync m) => t m String -> t m (Edge Integer)
parseEdges = S.mapM toEdge

-- generator :: (IsStream t, MonadAsync m) => t m (Edge Integer) -> t m Integer
-- generator s = SD.foldrS (generatorFilter' s) nil s

g :: FU.Unfold IO a b
g = undefined
--g = FU.mkUnfold (a -> m (Step a b))

generatorFilter' :: (IsStream t, MonadAsync m) => Edge Integer -> t m Integer -> t m Integer
generatorFilter' e@(Edge (a, _)) st = S.yield a <> st |& generatorFilter' e

-- generator :: (IsStream t, MonadAsync m) => t m (Edge Integer) -> t m (ConnectedComponents Integer)
-- generator = generator'' . SD.foldrS generatorFilter nil 

newGo :: (IsStream t, MonadAsync m) => t m (Edge Integer) -> t m (ConnectedComponents Integer)
newGo = SD.foldrS generator'' nil

generatorFilter :: (IsStream t, MonadAsync m) => Edge Integer -> t m (Edge Integer) -> t m (Edge Integer)
generatorFilter headElem prevStream = S.yield headElem <> prevStream |& S.map id

generator'' :: (IsStream t, MonadAsync m) => t m (Edge Integer) -> t m (ConnectedComponents Integer)
generator'' = S.foldMany (FL.mkFoldl (\b a-> if a `includedIncident` b || DC.null b then a `addToConnectedComp` b else b) mempty)
--generator'' :: (IsStream t, Monad (t m), MonadAsync m) => SerialT (t m) (Edge Integer) -> t m [ConnectedComponents Integer]
-- generator'' :: (IsStream t, Monad (t m)) => SerialT (t m) (Edge Integer) -> t m [ConnectedComponents Integer]
-- generator'' = S.fold (many (FL.mkFoldl (\b a-> if a `includedIncident` b || DC.null b then a `addToConnectedComp` b else b) mempty))

generator' :: (IsStream t, MonadAsync m) => t m (Edge Integer) -> t m (ConnectedComponents Integer)
generator' = SE.concatSmapMWith parallel newFilter' (pure mempty)

newFilter' :: (IsStream t, MonadAsync m)
           => ConnectedComponents Integer
           -> Edge Integer
           -> m (ConnectedComponents Integer, t m (ConnectedComponents Integer))
newFilter' xs edge
  | edge `includedIncident` xs
  = let newCC = edge `addToConnectedComp` xs in traceThreads edge >> return (newCC, S.yield newCC)
  | otherwise
  = traceThreads edge >> return (xs, S.yield xs)


--traceThreads :: (MonadIO m, Show b) => b -> m ()
traceThreads :: Applicative f => a -> f ()
traceThreads a = void $ pure a-- liftIO (myThreadId >>= R.putStrLn . mappend (show a) . show)


output :: (IsStream t, MonadAsync m) => t m (ConnectedComponents Integer) -> t m ()
output = S.mapM print

