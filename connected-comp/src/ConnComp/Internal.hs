{-# LANGUAGE  BlockArguments #-}
module ConnComp.Internal
  ( runDPConnectedComp
  ) where

--import           Control.Monad.Catch
import Control.Concurrent
import Control.Concurrent.Async
import Data.ByteString as B
import Control.Concurrent.STM.TChan as TC
import           Data.ConnComp                                     as DC
import           Relude                                            as R


type Stream = Maybe

runParallelDP :: Handle -> IO ()
runParallelDP h = do
  (input, a) <- fromInput h
  (in', b) <- map' toEdge input
  (out, as) <- generator in' 
  final <- map' R.putStrLn out
  mapM_ wait (a:b:final:as)

generator :: TChan (Stream (Edge Integer)) -> IO (TChan (Stream (ConnectedComponents Integer)), [Async ()])
generator = error "not implemented"

map' :: (a -> IO b) -> TChan (Stream a) -> IO (Async ())
map' f inCh = 
  async $ loop inCh 
  where 
    loop ic = do 
      e <- atomically (readTChan ic)
      maybe (pure ()) (\a -> f a >> loop ic) e

runDPConnectedComp :: IO ()
runDPConnectedComp = do
  file <- maybe (liftIO $ fail "Error no parameter found") return . R.viaNonEmpty R.head =<< liftIO getArgs
  R.withFile file ReadMode runParallelDP


toOutput :: TChan (Stream (ConnectedComponents Integer)) -> IO (Async ())
toOutput inCh =
  async $ loop inCh 
  where 
    loop ic = do 
      e <- atomically (readTChan ic)
      maybe (pure ()) (\a -> R.putStrLn (show a) >> loop ic) e


fromInput :: Handle -> IO (TChan (Stream ByteString), Async ())
fromInput h = do
  inputChannel <- newTChanIO
  a <- async $ loop inputChannel
  return (inputChannel, a)
 where
  loop inCh = do
    eof <- R.hIsEOF h
    if eof
        then atomically $ writeTChan inCh Nothing
        else do
            x <- B.hGetNonBlocking h (1024 * 256)
            atomically $ writeTChan inCh (Just x)
            loop inCh

        


  


