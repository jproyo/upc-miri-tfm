module Dynamic.Pipeline where

import qualified Control.Concurrent                                as CC
import           Control.Concurrent.Async
import           Control.Concurrent.Chan.Unagi.NoBlocking                                                      hiding ( Stream
                                                                                                                      )
import           Data.ByteString                                   as B
import           Relude                                                                                        hiding ( map
                                                                                                                      , mapM
                                                                                                                      , traverse
                                                                                                                      )
import qualified Relude                                            as R

type Channel a = (InChan (Maybe a), OutChan (Maybe a))

data Stream a b = Stream
  { inChannel  :: Channel a
  , outChannel :: Channel b
  , process    :: Async ()
  }

{-# INLINE end' #-}
end' :: Channel a -> IO ()
end' = flip writeChan Nothing . fst

{-# INLINE endIn #-}
endIn :: Stream a b -> IO ()
endIn = end' . inChannel

{-# INLINE endOut #-}
endOut :: Stream a b -> IO ()
endOut = end' . outChannel

{-# INLINE push' #-}
push' :: a -> Channel a -> IO ()
push' e = flip writeChan (Just e) . fst

{-# INLINE pushOut #-}
pushOut :: b -> Stream a b -> IO ()
pushOut e = push' e . outChannel

{-# INLINE pushIn #-}
pushIn :: a -> Stream a b -> IO ()
pushIn e = push' e . inChannel

{-# INLINE pull' #-}
pull' :: Channel a -> IO (Maybe a)
pull' = readChan (CC.threadDelay 10) . snd

{-# INLINE pullIn #-}
pullIn :: Stream a b -> IO (Maybe a)
pullIn = pull' . inChannel

{-# INLINE pullOut #-}
pullOut :: Stream a b -> IO (Maybe b)
pullOut = pull' . outChannel

{-# INLINE foldrS #-}
foldrS :: (Stream a b -> a -> IO (Stream a b)) -> Stream a b -> IO (Stream a b)
foldrS = loop
  where 
    loop fio c = maybe (return c) (loop fio <=< fio c) =<< pullIn c

{-# INLINE (|>>) #-}
(|>>) :: Stream a b -> (a -> IO c) -> IO (Stream c b)
(|>>) inp f = do
  newC' <- newChan
  newO' <- newChan
  end' newO'
  Stream newC' newO' <$> async (loop newC')
 where
  loop newCh = pullIn inp >>= loopUntilDone newCh (loopE newCh) loop

  loopE ch a = flip push' ch =<< f a

loopUntilDone :: Channel b -> (a -> IO ()) -> (Channel b -> IO ()) -> Maybe a -> IO ()
loopUntilDone ch f loop = maybe (end' ch) ((>> loop ch) . f)

-- Generate Stream base on a seed function `f`
{-# INLINE unfoldM #-}
unfoldM :: IO a -> IO Bool -> IO (Stream a b)
unfoldM f stop = do
  newCh  <- newChan
  newCh' <- newChan
  end' newCh'
  Stream newCh newCh' <$> async (loop newCh)
  where loop newCh = ifM stop (end' newCh) (f >>= (`push'` newCh) >> loop newCh)

{-# INLINE mapM #-}
mapM :: (b -> IO c) -> Stream a b -> IO ()
mapM f inCh = async loop >>= wait where loop = maybe (pure ()) (\a -> f a >> loop) =<< pullOut inCh

mapCount :: (Int -> IO ()) -> Int -> Stream a b -> IO ()
mapCount f i inCh = async (loop i) >>= wait where loop c = maybe (pure ()) (const $ f c >> loop (c+1)) =<< pullOut inCh

{-# INLINE foldMap #-}
foldMap :: Monoid m => (b -> m) -> Stream a b -> IO m
foldMap m s = async (loop mempty) >>= wait where loop xs = maybe (pure xs) (loop . mappend xs . m) =<< pullOut s

{-# INLINE newStream #-}
newStream :: IO (Async ()) -> IO (Stream a b)
newStream as = Stream <$> newChan <*> newChan <*> as

{-# INLINE fromText #-}
fromText :: Text -> IO (Stream ByteString b)
fromText bs = newStream (async $ pure ()) >>= \s -> R.mapM_ (`pushIn` s) (R.map R.encodeUtf8 $ R.lines bs) >> endIn s >> return s
