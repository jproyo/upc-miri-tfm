module ConnComp.Internal
  ( runDPConnectedComp
  , runParallelDP
  , runParallelDP'
  , program
  , DP.fromText
  ) where

-- import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.Chan.Unagi.NoBlocking
                                               as TC
import           Data.ByteString               as B
import           Data.ConnComp                 as DC
import qualified Dynamic.Pipeline              as DP
import           Dynamic.Pipeline               ( (|>>) )
import           Relude                        as R
import           Utils.Trace


type ConnCompDP = DP.Stream Edge ConnectedComponents

runParallelDP :: Handle -> IO ()
runParallelDP h = do
  now <- takeTime
  input h >>= generator now >>= output now

input :: Handle -> IO (DP.Stream Edge ConnectedComponents)
input h = fromInput h >>= (|>> parseEdges)

output :: Integer -> ConnCompDP -> IO ()
--output = (print . getSum) <=< DP.foldMap (const $ Sum (1::Int))
output now cc = do
  putBSLn "test,approach,answer,time"
  DP.mapCount printNext 1 cc
  where 
    printNext c = printCC "DP-WCC" c now =<< takeTime 

runParallelDP' :: DP.Stream ByteString ConnectedComponents
               -> IO [ConnectedComponents]
runParallelDP' sInput = do
  now <- takeTime 
  parseInput <- sInput |>> parseEdges
  out        <- generator now parseInput
  DP.foldMap (: []) out

generator :: Integer -> ConnCompDP -> IO ConnCompDP
generator now = DP.foldrS createNewFilter
 where
  createNewFilter c v = do
    newInput  <- TC.newChan
    newOutput <- TC.newChan
    DP.Stream newInput newOutput
      <$> async (newFilter now (toConnectedComp v) c newInput newOutput)

newFilter :: Integer -> ConnectedComponents
          -> ConnCompDP
          -> DP.Channel Edge
          -> DP.Channel ConnectedComponents
          -> IO ()
newFilter _ conn inCh toInCh outCh = 
  actor1 conn inCh toInCh >>= actor2 inCh toInCh outCh

actor1 :: ConnectedComponents
       -> ConnCompDP
       -> DP.Channel Edge
       -> IO ConnectedComponents
actor1 conn inCh toInCh = maybe finishActor doActor =<< DP.pullIn inCh
 where
  finishActor = DP.end' toInCh >> return conn

  doActor v
    | toConnectedComp v `intersect` conn = actor1 (toConnectedComp v <> conn)
                                                  inCh
                                                  toInCh
    | otherwise = v `DP.push'` toInCh >> actor1 conn inCh toInCh

actor2 :: ConnCompDP
       -> DP.Channel Edge
       -> DP.Channel ConnectedComponents
       -> ConnectedComponents
       -> IO ()
actor2 inCh toInCh outCh conn = maybe finishActor doActor =<< DP.pullOut inCh

 where
  finishActor = conn `DP.push'` outCh >> DP.end' outCh

  doActor cc | conn `intersect` cc = actor2 inCh toInCh outCh (conn <> cc)
             | otherwise = cc `DP.push'` outCh >> actor2 inCh toInCh outCh conn


runDPConnectedComp :: FilePath -> IO ()
runDPConnectedComp file = R.withFile file ReadMode runParallelDP

program :: FilePath -> IO ()
program = runDPConnectedComp

parseEdges :: ByteString -> IO Edge
parseEdges = toEdge . decodeUtf8

fromInput :: Handle -> IO (DP.Stream ByteString ConnectedComponents)
fromInput h = DP.unfoldM (B.hGetLine h) (R.hIsEOF h)

