{-# LANGUAGE  BlockArguments #-}
module ConnComp.Internal
  ( runDPConnectedComp
  ) where

import           Control.Concurrent.Async
import           Control.Concurrent.STM.TQueue                     as TC
import           Data.ByteString                                   as B
import           Data.ConnComp                                     as DC
import qualified Dynamic.Pipeline                                  as DP
import           Dynamic.Pipeline                                                                                     ( (-->)
                                                                                                                      , (|>>)
                                                                                                                      )
import           Relude                                            as R


runParallelDP :: Handle -> IO ()
runParallelDP h = do
  sInput     <- fromInput h -- Input 
  parseInput <- sInput |>> parseEdges
  out        <- newTQueueIO 
  as         <- generator parseInput out -- Generator
  final      <- DP.mapM (R.putStrLn . show) out -- Output
  DP.processStreams (DP.trigger sInput : DP.trigger parseInput : final : R.map DP.trigger as)

generator :: DP.Stream (Edge Integer) -> DP.Channel (ConnectedComponents Integer) -> IO [DP.Stream (Edge Integer)]
generator chn outChn = loop chn []
 where
  loop c xs = maybe (finishGenerator xs) (createNewFilter c xs) =<< DP.pull c

  finishGenerator xs = DP.end' outChn >> return xs

  createNewFilter c xs v = do
    newInput <- newTQueueIO
    s        <- DP.Stream newInput <$> async (newFilter (toConnectedComp v) c newInput outChn)
    loop s (s : xs)

newFilter :: ConnectedComponents Integer
          -> DP.Stream (Edge Integer)
          -> DP.Channel (Edge Integer)
          -> DP.Channel (ConnectedComponents Integer)
          -> IO ()
newFilter conn inCh toInCh outCh = maybe finishFilter processElem =<< DP.pull inCh

 where
  finishFilter = conn --> outCh >> DP.end' toInCh >> return ()

  processElem v
    | v `includedIncident` conn = do
      let newList = v `addToConnectedComp` conn -- Connected Comp is not a maximal 
      newFilter newList inCh toInCh outCh
    | otherwise = v --> toInCh >> newFilter conn inCh toInCh outCh



runDPConnectedComp :: IO ()
runDPConnectedComp = do
  file <- maybe (liftIO $ fail "Error no parameter found") return . R.viaNonEmpty R.head =<< liftIO getArgs
  R.withFile file ReadMode runParallelDP

parseEdges :: ByteString -> IO [Edge Integer]
parseEdges = toEdge . decodeUtf8

fromInput :: Handle -> IO (DP.Stream ByteString)
fromInput h = DP.unfoldM (B.hGetNonBlocking h (1024 * 256)) (R.hIsEOF h)




