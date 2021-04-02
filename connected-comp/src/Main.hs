module Main where

import Streamly
import Streamly.Internal.Data.Stream.StreamK as SD
import qualified Streamly.FileSystem.Handle as FH
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Memory.Array as SA
import qualified Streamly.Internal.Data.Unicode.Stream as US
import Streamly.Prelude as S
import Relude as R
import Control.Concurrent

type Edge a = (a,a)

newtype Edges a = Edges [Edge a]
  deriving (Functor, Show, Eq)

newtype ConnectedComponents a = ConnectedComponents [a]
  deriving newtype (Functor, Applicative, Show, Eq)

newtype Result a = Result (ConnectedComponents a)
  deriving newtype (Functor, Applicative, Show, Eq)

main :: IO ()
main =  setup >> prog

setup :: IO ()
setup = hSetBuffering stdout LineBuffering

prog :: IO ()
prog = do
  file <- maybe (fail "Error no parameter found") return . R.viaNonEmpty R.head =<< getArgs
  withFile file ReadMode $ \fh ->
    S.drain . aheadly $ runDPConnectedComp fh

runDPConnectedComp :: IsStream t => Handle -> t IO ()
runDPConnectedComp fh = input fh |& filterGenerator |& generator |& output

dataSource :: IsStream s => s IO Integer
dataSource = S.fromFoldable ([1..20]::[Integer])

input :: IsStream t => Handle -> t IO Integer
input h = S.unfold FH.readChunksWithBufferOf (256*1024, h) &
          S.concatMap SA.toStream &
          US.decodeUtf8 &
          US.lines FL.toList &
          S.map readEither &
          S.mapM (either (fail . show) pure)         

filterGenerator :: IsStream s => s IO Integer -> s IO Integer
filterGenerator = SD.foldrS (\a b -> if even a then S.yield a <> b |& newFilter a else S.yield a <> b) nil

newFilter :: IsStream s => Integer -> s IO Integer -> s IO Integer
newFilter = S.mapM . filtering

filtering :: Integer -> Integer -> IO Integer
filtering y x = do
  tid <- myThreadId
  R.putStrLn . mappend (show x) . mappend (" - Filter " <> show y <> " - ") . show $ tid
  if x == 2 then threadDelay 1000000 else pure ()
  return (if x == y then x+20 else x)

generator :: IsStream s => s IO Integer -> s IO Integer
generator = S.mapM return
  -- where
  --   generating x = do 
  --     tid <- myThreadId 
  --     R.putStrLn . mappend (show x) . mappend " - " . show $ tid
  --     return x

output :: IsStream s => s IO Integer -> s IO ()
output = S.mapM print

