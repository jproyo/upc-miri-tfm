module Dynamic.Pipeline where

import           Control.Concurrent.Async
import           Control.Concurrent.Chan.Unagi
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

end' :: Channel a -> IO ()
end' = flip writeChan Nothing . fst

endIn :: Stream a b -> IO ()
endIn = end' . inChannel

endOut :: Stream a b -> IO ()
endOut = end' . outChannel

push' :: a -> Channel a -> IO ()
push' e s = writeChan (fst s) (Just e)

pushOut :: b -> Stream a b -> IO ()
pushOut e = push' e . outChannel

pushIn :: a -> Stream a b -> IO ()
pushIn e = push' e . inChannel

pull' :: Channel a -> IO (Maybe a)
pull' = readChan . snd

pullIn :: Stream a b -> IO (Maybe a)
pullIn = pull' . inChannel

pullOut :: Stream a b -> IO (Maybe b)
pullOut = pull' . outChannel

(|>>) :: Foldable f => Stream a b -> (a -> IO (f c)) -> IO (Stream c b)
(|>>) inp f = do
  newC' <- newChan
  newO' <- newChan
  end' newO'
  Stream newC' newO' <$> async (loop newC')
 where
  loop newCh = pullIn inp >>= loopUntilDone newCh (loopE newCh) loop

  loopE newCh a = do
    newVal <- f a
    void $ foldMapM (`push'` newCh) newVal

loopUntilDone :: Channel b -> (a -> IO ()) -> (Channel b -> IO ()) -> Maybe a -> IO ()
loopUntilDone ch f loop = maybe (end' ch) ((>> loop ch) . f)

-- Generate Stream base on a seed function `f`
unfoldM :: IO a -> IO Bool -> IO (Stream a b)
unfoldM f stop = do
  newCh  <- newChan
  newCh' <- newChan
  end' newCh'
  Stream newCh newCh' <$> async (loop newCh)
  where loop newCh = ifM stop (end' newCh) (f >>= (`push'` newCh) >> loop newCh)


mapM :: (b -> IO c) -> Stream a b -> IO (Async ())
mapM f inCh = async loop
 where
  loop = do
    e <- pullOut inCh
    maybe (pure ()) (\a -> f a >> loop) e


processStreams :: [Async ()] -> IO ()
processStreams = mapConcurrently_ wait

newStream :: IO (Async ()) -> IO (Stream a b)
newStream as = Stream <$> newChan <*> newChan <*> as