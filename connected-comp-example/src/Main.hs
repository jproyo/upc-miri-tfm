module Main where

import Streamly
import Streamly.Internal.Prelude as SI
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
dynamicPipeline s = s |& input |& generator |& output

dataSource :: IsStream s => s IO Integer
dataSource = S.fromFoldable ([1..20]::[Integer]) 

input :: IsStream s => s IO Integer -> s IO Integer
input = fmap id

generator :: IsStream s => s IO Integer -> s IO Integer 
generator s = SI.foldrS (\a b -> if even a then b |& newFilter a else b) (generator' s) s

newFilter :: IsStream s => Integer -> s IO Integer -> s IO Integer
newFilter y = S.mapM (\x -> myThreadId >>= (R.putStrLn . mappend (show x) . mappend (" - Filter " <> show y <> " - ") . show) >> return (if x == y then x+20 else x))

generator' :: IsStream s => s IO Integer -> s IO Integer
generator' = S.mapM (\x -> myThreadId >>= (R.putStrLn . mappend (show x) . mappend " - " . show) >> return x)

output :: IsStream s => s IO Integer -> s IO ()
output = S.mapM print

