{-# LANGUAGE  BlockArguments #-}
module ConnComp.Internal
  ( runDPConnectedComp
  ) where

--import           Control.Monad.Catch
import Control.Concurrent.Async
import Data.ByteString as B
import Control.Concurrent.STM.TChan as TC
import           Data.ConnComp                                     as DC
import           Relude                                            as R


type Stream = Maybe

runParallelDP :: Handle -> IO ()
runParallelDP h = do
  (input, a) <- fromInput h
  (in', b) <- input |>> parseEdges
  (out, as) <- generator in' =<< newTChanIO 
  final <- mapM' (R.putStrLn . show) out
  mapM_ wait (a:b:final:as)

generator :: TChan (Stream (Edge Integer)) -> TChan (Stream (ConnectedComponents Integer)) -> IO (TChan (Stream (ConnectedComponents Integer)), [Async ()])
generator chn outChn = loop chn []
  where
    loop c xs = do 
      e <- pull c
      case e of 
        Nothing -> end outChn >> return (outChn, xs)
        Just v  -> do
          newInput  <- newTChanIO 
          a <- async $ newFilter (toConnectedComp v) c newInput outChn
          loop newInput (a:xs)

newFilter :: ConnectedComponents Integer -> TChan (Stream (Edge Integer)) -> TChan (Stream (Edge Integer)) -> TChan (Stream (ConnectedComponents Integer)) -> IO ()
newFilter conn inCh toInCh outCh = do
  e <- pull inCh
  case e of 
    Nothing -> conn >-> outCh >> return ()
    Just v  ->
      if v `includedIncident` conn 
        then do 
          let newList = v `addToConnectedComp` conn
          newFilter newList inCh toInCh outCh
        else do 
          v >-> toInCh
          newFilter conn inCh toInCh outCh

dup :: TChan (Stream a) -> IO (TChan (Stream a))
dup inCh = do 
  newCh <- newTChanIO 
  loop newCh
  return newCh
  where 
    loop newCh = do 
      e <- pull inCh
      maybe (end newCh) (\a -> a >-> newCh >> loop newCh) e 

mapM' :: (a -> IO b) -> TChan (Stream a) -> IO (Async ())
mapM' f inCh = 
  async loop 
  where 
    loop = do 
      e <- pull inCh
      maybe (pure ()) (\a -> f a >> loop) e

end :: TChan (Stream a) -> IO ()
end c = atomically $ writeTChan c Nothing

(>->) :: a -> TChan (Stream a) -> IO ()
(>->) e c = atomically $ writeTChan c $ Just e

pull :: TChan (Stream a) -> IO (Maybe a)
pull c = atomically $ readTChan c

(|>) :: TChan (Stream a) -> (a -> IO b) -> IO (TChan (Stream b), Async ())
(|>) inp f = do 
  newC <- newTChanIO
  a <- async $ loop newC
  return (newC, a)
  where 
    loop newCh = do 
      e <- pull inp
      maybe (end newCh) (\a -> loopE newCh a >> loop newCh) e

    loopE newCh a = do
      newVal <- f a
      newVal >-> newCh

(|>>) :: TChan (Stream a) -> (a -> IO [b]) -> IO (TChan (Stream b), Async ())
(|>>) inp f = do 
  newC' <- newTChanIO
  a <- async $ loop newC'
  return (newC', a)
  where 
    loop newCh = do 
      e <- pull inp
      maybe (end newCh) (\a -> loopE newCh a >> loop newCh) e

    loopE newCh a = do
      newVal <- f a
      mapM_ (>-> newCh) newVal 
      
runDPConnectedComp :: IO ()
runDPConnectedComp = do
  file <- maybe (liftIO $ fail "Error no parameter found") return . R.viaNonEmpty R.head =<< liftIO getArgs
  R.withFile file ReadMode runParallelDP

parseEdges :: ByteString -> IO [Edge Integer]
parseEdges = toEdge . decodeUtf8

fromInput :: Handle -> IO (TChan (Stream ByteString), Async ())
fromInput h = do
  inputChannel <- newTChanIO
  a <- async $ loop inputChannel
  return (inputChannel, a)
 where
  loop inCh = do
    eof <- R.hIsEOF h
    if eof
        then end inCh
        else do
            x <- B.hGetNonBlocking h (1024 * 256)
            x >-> inCh
            loop inCh

        


  


