module Dynamic.Pipeline where

import qualified Control.Concurrent as CC
import           Control.Concurrent.Async
import           Control.Concurrent.Chan.Unagi.NoBlocking                                                      hiding ( Stream
                                                                                                                      )
import           Relude                                                                                        hiding ( map
                                                                                                                      , mapM
                                                                                                                      , traverse
                                                                                                                      )

type Channel a = (InChan (Maybe a), OutChan (Maybe a))

data Stream a b = Stream
  { inChannel  :: Channel a
  , outChannel :: Channel b
  , trigger    :: Async ()
  }

class AsyncWait f where
    toAsync :: f -> Async ()

instance AsyncWait (Stream a b) where
  toAsync = trigger

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
pull' = readChan (CC.threadDelay 100) . snd

{-# INLINE pullIn #-}
pullIn :: Stream a b -> IO (Maybe a)
pullIn = pull' . inChannel

{-# INLINE pullOut #-}
pullOut :: Stream a b -> IO (Maybe b)
pullOut = pull' . outChannel

{-# INLINE (|>>) #-}
(|>>) :: Stream a b -> (a -> IO [c]) -> IO (Stream c b)
(|>>) inp f = do
  newC' <- newChan
  newO' <- newChan
  end' newO'
  Stream newC' newO' <$> async (loop newC')
 where
  loop newCh = pullIn inp >>= loopUntilDone newCh (loopE newCh) loop

  loopE (inC, _) a = writeList2Chan inC . fmap Just =<< f a

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
mapM :: (b -> IO c) -> Stream a b -> IO (Async ())
mapM f inCh = async loop where loop = maybe (pure ()) (\a -> f a >> loop) =<< pullOut inCh

{-# INLINE fold #-}
fold :: Stream a b -> IO [b]
fold s = loop [] where loop xs = maybe (pure xs) (loop . (: xs)) =<< pullOut s

{-# INLINE processStreams #-}
processStreams :: [Async ()] -> IO ()
processStreams = mapConcurrently_ wait

{-# INLINE newStream #-}
newStream :: IO (Async ()) -> IO (Stream a b)
newStream as = Stream <$> newChan <*> newChan <*> as

{-# INLINE fromByteString #-}
fromByteString :: ByteString -> IO (Stream ByteString b)
fromByteString bs = newStream (async $ pure ()) >>= \s -> pushIn bs s >> endIn s >> return s
