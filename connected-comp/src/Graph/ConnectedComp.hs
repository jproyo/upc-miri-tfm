-- |
-- Module      : Graph.ConnectedComp
-- Copyright   : (c) 2021 Juan Pablo Royo Sales
--
-- License     : BSD3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : experimental
-- Portability : GHC
module Graph.ConnectedComp where

import           DynamicPipeline
import           Graph.ConnComp
import           Relude
import           Utils.Trace

-- brittany-disable-next-binding
type DPConnComp = Source (Channel (Edge :<+> ConnectedComponents :<+> Eof))
                :=> Generator (Channel (Edge :<+> ConnectedComponents :<+> Eof))
                :=> Sink

source' :: FilePath
        -> Stage
           (WriteChannel Edge -> WriteChannel ConnectedComponents -> DP st ())
source' filePath = withSource @DPConnComp
  $ \edgeOut _ -> unfoldFile filePath edgeOut (toEdge . decodeUtf8)

sink' :: Integer -> Stage (ReadChannel Edge -> ReadChannel ConnectedComponents -> DP st ())
sink' initialTime = 
  withSink @DPConnComp $ \_ cc -> withDP $ do 
    c  <- newIORef 1
    putTextLn "test,approach,answer,time"
    foldM_ cc (const $ treatElement c initialTime)
  where
    treatElement c initialTime' = do 
      c' <- readIORef c
      printCC "DPFH-WCC" c' initialTime' =<< takeTime
      modifyIORef c (+ 1)



generator' :: GeneratorStage DPConnComp ConnectedComponents Edge st
generator' =
  let gen = withGenerator @DPConnComp genAction
  in  mkGenerator gen filterTemplate

filterTemplate :: Filter DPConnComp ConnectedComponents Edge st
filterTemplate = actor actor1 |>> actor actor2

actor1 :: Edge
       -> ReadChannel Edge
       -> ReadChannel ConnectedComponents
       -> WriteChannel Edge
       -> WriteChannel ConnectedComponents
       -> StateT ConnectedComponents (DP st) ()
actor1 _ readEdge _ writeEdge _ = 
  foldM_ readEdge $ \e -> get >>= doActor e
 where
  doActor v conn
    | toConnectedComp v `intersect` conn = modify' (toConnectedComp v <>)
    | otherwise = push v writeEdge

actor2 :: Edge
       -> ReadChannel Edge
       -> ReadChannel ConnectedComponents
       -> WriteChannel Edge
       -> WriteChannel ConnectedComponents
       -> StateT ConnectedComponents (DP st) ()
actor2 _ _ readCC _ writeCC = do 
  foldWithM_ readCC pushMemory $ \e -> get >>= doActor e

 where
   pushMemory = get >>= flip push writeCC

   doActor cc conn
    | cc `intersect` conn = modify' (cc <>)
    | otherwise = push cc writeCC


genAction :: Filter DPConnComp ConnectedComponents Edge st
          -> ReadChannel Edge
          -> ReadChannel ConnectedComponents
          -> WriteChannel Edge
          -> WriteChannel ConnectedComponents
          -> DP st ()
genAction filter' readEdge readCC _ writeCC = do
  let unfoldFilter = mkUnfoldFilterForAll filter' toConnectedComp readEdge (readCC .*. HNil) 
  results <- unfoldF unfoldFilter
  foldM_ (hHead results) (`push` writeCC)


program :: FilePath -> IO ()
program file = do 
  now <- takeTime
  runDP $ mkDP @DPConnComp (source' file) generator' (sink' now)
