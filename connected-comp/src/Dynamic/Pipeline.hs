module Dynamic.Pipeline where

import           Control.Concurrent.Async
import           Control.Concurrent.STM.TQueue                     as TC
import           Relude                                                                                        hiding ( map
                                                                                                                      , mapM
                                                                                                                      , traverse
                                                                                                                      )

type Channel a = TQueue (Maybe a)

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
end' = atomically . flip writeTQueue Nothing

(>->) :: a -> Stream a -> IO ()
(>->) e = (e -->) . channel

(-->) :: a -> Channel a -> IO ()
(-->) e s = atomically . writeTQueue s $ Just e

pull :: Stream a -> IO (Maybe a)
pull = pull' . channel

pull' :: Channel a -> IO (Maybe a)
pull' = atomically . readTQueue

(|>>) :: Foldable f => Stream a -> (a -> IO (f b)) -> IO (Stream b)
(|>>) inp f = do
  newC' <- newTQueueIO
  Stream newC' <$> async (loop newC')
 where
  loop newCh = pull inp >>= loopUntilDone newCh (loopE newCh) loop

  loopE newCh a = do
    newVal <- f a
    void $ atomically $ foldMapM (writeTQueue newCh . Just) newVal

loopUntilDone :: Channel b -> (a -> IO ()) -> (Channel b -> IO ()) -> Maybe a -> IO ()
loopUntilDone ch f loop = maybe (end' ch) ((>> loop ch) . f)

-- Generate Stream base on a seed function `f`
unfoldM :: IO b -> IO Bool -> IO (Stream b)
unfoldM f stop = do
  newCh <- newTQueueIO
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

