module Main where

import Streamly
import Streamly.Internal.Data.Stream.StreamK as SD
import Streamly.Prelude as S
import Relude as R
import System.IO (hSetBuffering, BufferMode(LineBuffering))
import Control.Concurrent


type Edge a = (a,a)


main :: IO ()
main = hSetBuffering stdout LineBuffering >> prog
    
prog :: IO ()
prog = S.drain 
     $ asyncly
     $ dynamicPipeline dataSource

dynamicPipeline :: IsStream t => t IO Integer -> t IO ()
dynamicPipeline s = s |& input |& filterGenerator |& generator |& output

dataSource :: IsStream s => s IO Integer
dataSource = S.fromFoldable ([1..20]::[Integer]) 

input :: IsStream s => s IO Integer -> s IO Integer
input = fmap id

filterGenerator :: IsStream s => s IO Integer -> s IO Integer 
filterGenerator s = SD.foldlS (\b a -> if even a then b |& newFilter a else b) s s

newFilter :: IsStream s => Integer -> s IO Integer -> s IO Integer
newFilter = S.mapM . filtering

--filtering :: (Eq b, Num b, Show b) => b -> b -> IO b
filtering :: Integer -> Integer -> IO Integer
filtering y x = do 
  if x == 2 then threadDelay 1000000 else pure ()
  -- tid <- myThreadId
  -- R.putStrLn . mappend (show x) . mappend (" - Filter " <> show y <> " - ") . show $ tid
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

