module Main where

import Streamly
import Streamly.Prelude as S
import Streamly.Internal.Prelude as SI
import Relude as R
import Control.Concurrent
import System.IO (hSetBuffering, BufferMode(LineBuffering))

data Prog = Continue | Eof
  deriving (Show, Eq)

main :: IO ()
main = hSetBuffering stdout LineBuffering >> prog
    
prog :: IO ()
prog = 
    S.drainWhile (/= Eof)
     $ S.mapM (\(x,s) -> myThreadId >>= putTextLn . mappend ("Output Value: " <> show x <> " - Signal: " <> show s <> " - ThreadId: ") . show >> return s)
    |$ S.mapM (\(x,s) -> myThreadId >>= putTextLn . mappend ("Filter " <> show x <> " Value: " <> show x <> " - Signal: " <> show s <> " - ThreadId: ") . show >> return (if x == 5 || s == Eof then (x,Eof) else (x,Continue)))
    |$ S.mapM (\(x::Integer) -> myThreadId >>= putTextLn . mappend ("Input Value: " <> show x <> " - ThreadId: ") . show >> return (if x == 5 then (x,Eof) else (x+1,Continue)))
    |$ S.enumerateFromTo 1 100
  


