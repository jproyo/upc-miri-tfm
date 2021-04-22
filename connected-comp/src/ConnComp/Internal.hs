module ConnComp.Internal
  ( runDPConnectedComp
  , runParallelDP
  , runParallelDP'
  , DP.fromText
  ) where

import           Control.Concurrent.Async
import           Control.Concurrent.Chan.Unagi.NoBlocking          as TC
import           Data.ByteString                                   as B
import           Data.ConnComp                                     as DC
import qualified Dynamic.Pipeline                                  as DP
import           Dynamic.Pipeline                                                                                     ( (|>>)
                                                                                                                      )
import           Relude                                            as R


type ConnCompDP = DP.Stream Edge ConnectedComponents

runParallelDP :: Handle -> IO ()
runParallelDP h = input h >>= generator >>= output

input :: Handle -> IO (DP.Stream Edge ConnectedComponents)
input h = fromInput h >>= (|>> parseEdges)

output :: ConnCompDP -> IO ()
output = DP.mapM (R.putStrLn . show)

runParallelDP' :: DP.Stream ByteString ConnectedComponents -> IO [ConnectedComponents]
runParallelDP' sInput = do
  parseInput <- sInput |>> parseEdges
  out        <- generator parseInput
  DP.foldMap (: []) out

generator :: ConnCompDP -> IO ConnCompDP
generator = DP.foldrS createNewFilter
 where
  createNewFilter c v = do
    newInput  <- newChan
    newOutput <- newChan
    DP.Stream newInput newOutput <$> async (newFilter (toConnectedComp v) c newInput newOutput)

newFilter :: ConnectedComponents
          -> ConnCompDP
          -> DP.Channel Edge
          -> DP.Channel ConnectedComponents
          -> IO ()
newFilter conn inCh toInCh outCh = actor1 conn inCh toInCh >>= actor2 inCh toInCh outCh

actor1 :: ConnectedComponents -> ConnCompDP -> DP.Channel Edge -> IO ConnectedComponents
actor1 conn inCh toInCh = maybe finishActor doActor =<< DP.pullIn inCh
 where
  finishActor = DP.end' toInCh >> return conn

  doActor v
    | v `includedIncident` conn = do
      let newList = v `addToConnectedComp` conn
      actor1 newList inCh toInCh
    | otherwise = v `DP.push'` toInCh >> actor1 conn inCh toInCh


actor2 :: ConnCompDP
       -> DP.Channel Edge
       -> DP.Channel ConnectedComponents
       -> ConnectedComponents
       -> IO ()
actor2 inCh toInCh outCh conn = maybe finishActor doActor =<< DP.pullOut inCh

 where
  finishActor = conn `DP.push'` outCh >> DP.end' outCh

  doActor cc | conn `intersect` cc = let newCC = conn `union` cc in actor2 inCh toInCh outCh newCC
             | otherwise           = cc `DP.push'` outCh >> actor2 inCh toInCh outCh conn


runDPConnectedComp :: IO ()
runDPConnectedComp = do
  file <- maybe (fail "Error no parameter found") return . R.viaNonEmpty R.head =<< getArgs
  R.withFile file ReadMode runParallelDP

parseEdges :: ByteString -> IO Edge
parseEdges = toEdge . decodeUtf8

fromInput :: Handle -> IO (DP.Stream ByteString ConnectedComponents)
fromInput h = DP.unfoldM (B.hGetLine h) (R.hIsEOF h)

