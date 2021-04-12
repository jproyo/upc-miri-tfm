module Dynamic.Pipeline where

import           Control.Concurrent.Async
--import           Control.Concurrent.STM.TQueue                     as TC
import Control.Concurrent.Chan.Unagi
import           Relude                                                                                        hiding ( map
                                                                                                                      , mapM
                                                                                                                      , traverse
                                                                                                                      )

type Channel a = (InChan (Maybe a), OutChan (Maybe a))

data Stream a = Stream
  { channel :: Channel a
  , trigger :: Async ()
  }

class AsyncWait f where
    toAsync :: f -> Async ()

instance AsyncWait (Stream a) where
  toAsync = trigger

end :: Stream a -> IO ()
end = end' . channel

end' :: Channel a -> IO ()
end' = flip writeChan Nothing . fst

(>->) :: a -> Stream a -> IO ()
(>->) e = (e -->) . channel

(-->) :: a -> Channel a -> IO ()
(-->) e s = writeChan (fst s) (Just e)

pull :: Stream a -> IO (Maybe a)
pull = pull' . channel

pull' :: Channel a -> IO (Maybe a)
pull' = readChan . snd

(|>>) :: Foldable f => Stream a -> (a -> IO (f b)) -> IO (Stream b)
(|>>) inp f = do
  newC' <- newChan
  Stream newC' <$> async (loop newC')
 where
  loop newCh = pull inp >>= loopUntilDone newCh (loopE newCh) loop

  loopE newCh a = do
    newVal <- f a
    void $ foldMapM (--> newCh) newVal

loopUntilDone :: Channel b -> (a -> IO ()) -> (Channel b -> IO ()) -> Maybe a -> IO ()
loopUntilDone ch f loop = maybe (end' ch) ((>> loop ch) . f)

-- Generate Stream base on a seed function `f`
unfoldM :: IO b -> IO Bool -> IO (Stream b)
unfoldM f stop = do
  newCh <- newChan
  Stream newCh <$> async (loop newCh)
  where loop newCh = ifM stop (end' newCh) (f >>= (--> newCh) >> loop newCh)


mapM :: (a -> IO b) -> Channel a -> IO (Async ())
mapM f inCh = async loop
 where
  loop = do
    e <- pull' inCh
    maybe (pure ()) (\a -> f a >> loop) e


processStreams :: [Async ()] -> IO ()
processStreams = mapConcurrently_ wait

