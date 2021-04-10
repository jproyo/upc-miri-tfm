{-# LANGUAGE  BlockArguments #-}
module ConnComp.Internal
  ( runDPConnectedComp
  ) where

--import           Control.Monad.Catch
import Control.Concurrent.Async
import Data.ByteString as B
import Control.Concurrent.STM.TChan as TC
--import           Data.ConnComp                                     as DC
import           Relude                                            as R
import GHC.IO.Handle


runParallelDP :: Handle -> IO ()
runParallelDP h = do
  (input, a) <- fromInput h
  b <- toOutput input
  mapM_ wait [a,b]

runDPConnectedComp :: IO ()
runDPConnectedComp = do
  file <- maybe (liftIO $ fail "Error no parameter found") return . R.viaNonEmpty R.head =<< liftIO getArgs
  R.withFile file ReadMode runParallelDP


toOutput :: TChan ByteString -> IO (Async ())
toOutput inCh =
  async $ loop inCh 
  where 
    loop ic = do 
      whenM (atomically $ isEmptyTChan ic) $ return ()
      atomically (readTChan ic) >>= print
      loop ic


fromInput :: Handle -> IO (TChan ByteString, Async ())
fromInput h = do
  inputChannel <- newTChanIO
  a <- async $ loop h inputChannel
  return (inputChannel, a)
 where
  loop h' inCh = do
    let bol = getAny <$> foldMapM (fmap Any) [hIsClosed h, R.hIsEOF h]
    whenM bol $ return ()
    whenM (not <$> bol) $ do
      x <- hGetSome h' (1024 * 256)
      atomically $ writeTChan inCh x
      loop h' inCh
        


  


