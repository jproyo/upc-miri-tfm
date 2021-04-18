module ConnComp.Internal
  ( runDPConnectedComp
  , runParallelDP
  , runParallelDP'
  , DP.fromByteString
  ) where

import           Control.Concurrent.Async
import           Control.Concurrent.Chan.Unagi.NoBlocking          as TC
import           Data.ByteString                                   as B
import           Data.ConnComp                                     as DC
import qualified Dynamic.Pipeline                                  as DP
import           Dynamic.Pipeline                                                                                     ( (|>>)
                                                                                                                      )
import           Relude                                            as R


type ConnCompDP = DP.Stream (Edge Integer) (ConnectedComponents Integer)

runParallelDP :: Handle -> IO ()
runParallelDP h = do
  sInput     <- fromInput h -- Input 
  parseInput <- sInput |>> parseEdges
  (out, as)  <- generator parseInput
  final      <- DP.mapM (R.putStrLn . show) out -- Output
  DP.processStreams (DP.trigger sInput : DP.trigger parseInput : final : DP.trigger out : R.map DP.trigger as)

runParallelDP' :: DP.Stream ByteString (ConnectedComponents Integer) -> IO [ConnectedComponents Integer]
runParallelDP' sInput = do
  parseInput <- sInput |>> parseEdges
  (out, as)  <- generator parseInput
  result     <- DP.fold out
  DP.processStreams (DP.trigger sInput : DP.trigger parseInput : DP.trigger out : R.map DP.trigger as)
  return result


generator :: ConnCompDP -> IO (ConnCompDP, [ConnCompDP])
generator chn = loop (chn, [])
 where
  loop (c, xs) = maybe (finishGenerator c xs) (createNewFilter c xs) =<< DP.pullIn c

  finishGenerator c xs = return (c, xs)

  createNewFilter c xs v = do
    newInput  <- newChan
    newOutput <- newChan
    s         <- DP.Stream newInput newOutput <$> async (newFilter (toConnectedComp v) c newInput newOutput)
    loop (s, s : xs)

newFilter :: ConnectedComponents Integer
          -> ConnCompDP
          -> DP.Channel (Edge Integer)
          -> DP.Channel (ConnectedComponents Integer)
          -> IO ()
newFilter conn inCh toInCh outCh = actor1 conn inCh toInCh >>= actor2 inCh toInCh outCh

actor1 :: ConnectedComponents Integer -> ConnCompDP -> DP.Channel (Edge Integer) -> IO (ConnectedComponents Integer)
actor1 conn inCh toInCh = maybe finishActor doActor =<< DP.pullIn inCh
 where
  finishActor = DP.end' toInCh >> return conn

  doActor v
    | v `includedIncident` conn = do
      let newList = v `addToConnectedComp` conn
      actor1 newList inCh toInCh
    | otherwise = v `DP.push'` toInCh >> actor1 conn inCh toInCh


actor2 :: ConnCompDP
       -> DP.Channel (Edge Integer)
       -> DP.Channel (ConnectedComponents Integer)
       -> ConnectedComponents Integer
       -> IO ()
actor2 inCh toInCh outCh conn = maybe finishActor doActor =<< DP.pullOut inCh

 where
  finishActor = conn `DP.push'` outCh >> DP.end' outCh

  doActor cc | conn `intersect` cc = let newCC = conn `union` cc in actor2 inCh toInCh outCh newCC
             | otherwise           = cc `DP.push'` outCh >> actor2 inCh toInCh outCh conn


runDPConnectedComp :: IO ()
runDPConnectedComp = do
  file <- maybe (liftIO $ fail "Error no parameter found") return . R.viaNonEmpty R.head =<< liftIO getArgs
  R.withFile file ReadMode runParallelDP

parseEdges :: ByteString -> IO [Edge Integer]
parseEdges = toEdge . decodeUtf8

fromInput :: Handle -> IO (DP.Stream ByteString (ConnectedComponents Integer))
fromInput h = DP.unfoldM (B.hGetNonBlocking h (1024 * 256)) (R.hIsEOF h)

