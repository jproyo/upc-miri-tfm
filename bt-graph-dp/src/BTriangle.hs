-- |
-- Module      : BTriangle
-- Copyright   : (c) 2021 Juan Pablo Royo Sales
--
-- License     : BSD3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : experimental
-- Portability : GHC
module BTriangle where

import           Control.Concurrent.Async
import           Data.IntSet                                       as IS
import           Data.Set                                          as S
import           DynamicPipeline
import           Edges
import           Relude                                            as R

-- brittany-disable-next-binding
type DPBT = Source (Channel (Edge :<+> W :<+> Q :<+> BT :<+> W :<+> Eof))
                :=> Generator (Channel (Edge :<+> W :<+> Q :<+> BT :<+> Eof))
                :=> FeedbackChannel (W :<+> Eof)
                :=> Sink

source' :: forall k (st :: k)
         . FilePath
        -> Stage
             (  ReadChannel W
             -> WriteChannel (UpperVertex, LowerVertex)
             -> WriteChannel W
             -> WriteChannel Q
             -> WriteChannel BT
             -> WriteChannel W
             -> DP st ()
             )
source' = withSource @DPBT . toFilters

toFilters :: FilePath
          -> ReadChannel W
          -> WriteChannel (UpperVertex, LowerVertex)
          -> WriteChannel W
          -> WriteChannel Q
          -> WriteChannel BT
          -> WriteChannel W
          -> DP st ()
toFilters filePath rfw wedges ww1 query wbt wfb = do
  unfoldFile filePath wedges (toEdge . decodeUtf8) -- Read from file and feed stream
  finish ww1 >> finish wbt -- mark as not used to continue in following filters
  rfw |=> wfb -- feedback channel
  push (ByVertex 101) query
  finish query


sink' :: forall k (st :: k)
       . Stage
        (ReadChannel (UpperVertex, LowerVertex) -> ReadChannel W -> ReadChannel Q -> ReadChannel BT -> DP st ())
sink' = withSink @DPBT toOutput

toOutput :: ReadChannel (UpperVertex, LowerVertex) -> ReadChannel W -> ReadChannel Q -> ReadChannel BT -> DP st ()
toOutput _ _ _ rbt = foldM_ rbt print

type FilterState = (W, DWTT, BTTT)

generator' :: forall k (st :: k) . GeneratorStage DPBT FilterState Edge st
generator' = let gen = withGenerator @DPBT genAction in mkGenerator gen filterTemplate

genAction :: forall k (st :: k)
           . Filter DPBT FilterState Edge st
          -> ReadChannel (UpperVertex, LowerVertex)
          -> ReadChannel W
          -> ReadChannel Q
          -> ReadChannel BT
          -> ReadChannel W
          -> WriteChannel (UpperVertex, LowerVertex)
          -> WriteChannel W
          -> WriteChannel Q
          -> WriteChannel BT
          -> WriteChannel W
          -> DP st ()
genAction filter' redges rw1 rq rbt rfd _ _ _ wbt wfc = do
  let unfoldFilter = mkUnfoldFilterForAll filter'
                                          (\(u, l) -> (W l $ IS.singleton u, mempty, mempty))
                                          redges
                                          (rw1 .*. rq .*. rbt .*. rfd .*. HNil)
  HCons rw1' (HCons _ (HCons rbt' _)) <- unfoldF unfoldFilter
  rw1' |=>| wfc $ id
  rbt' |=>| wbt $ id

filterTemplate :: forall k (st :: k) . Filter DPBT FilterState Edge st
filterTemplate = actor actor1 |>>> actor actor2 |>>> actor actor3 |>> actor actor4

actor1 :: Edge
       -> ReadChannel (UpperVertex, LowerVertex)
       -> ReadChannel W
       -> ReadChannel Q
       -> ReadChannel BT
       -> ReadChannel W
       -> WriteChannel (UpperVertex, LowerVertex)
       -> WriteChannel W
       -> WriteChannel Q
       -> WriteChannel BT
       -> WriteChannel W
       -> StateT FilterState (DP st) ()
actor1 (_, l) redges _ _ _ _ we ww1 _ _ _ = do
  foldM_ redges $ \e@(u', l') -> if l' == l
    then modify $ \(w@W {..}, dwtt, bttt) -> (w { _wWedges = addWedge _wWedges u' }, dwtt, bttt)
    else push e we
  finish we
  (w@(W _ w_t), _, _) <- get
  when (IS.size w_t > 1) $ push w ww1

actor2 :: Edge
       -> ReadChannel (UpperVertex, LowerVertex)
       -> ReadChannel W
       -> ReadChannel Q
       -> ReadChannel BT
       -> ReadChannel W
       -> WriteChannel (UpperVertex, LowerVertex)
       -> WriteChannel W
       -> WriteChannel Q
       -> WriteChannel BT
       -> WriteChannel W
       -> StateT FilterState (DP st) ()
actor2 (_, l) _ rw1 _ _ _ _ ww1 _ _ _ = do
  (W _ w_t, _, _) <- get
  foldM_ rw1 $ \w@(W l' w_t') -> do
    -- putTextLn $ "[A2] - Reading rw1: " <> show w_t'
    push w ww1
    if (IS.size w_t > 1) && l' < l && IS.size (IS.intersection w_t w_t') > 0
      then
        let
          si = w_t' IS.\\ w_t
          sj = IS.intersection w_t w_t'
          sk = w_t IS.\\ w_t'
          buildUt si' sj' sk' =
            [ [ (i, j, k) | k <- IS.toList sk', k > j ] | i <- IS.toList si', j <- IS.toList sj', i < j ]
          cond_a = IS.size si >= 1 && IS.size sj > 0 && IS.size sk > 0
          cond_b = IS.size si == 0 && IS.size sj > 1 && IS.size sk > 0
          cond_c = IS.size si == 0 && IS.size sj > 2 && IS.size sk == 0
          result | cond_a    = buildUt si sj sk
                 | cond_b    = buildUt sj sj sk
                 | cond_c    = buildUt sj sj sj
                 | otherwise = []
          ut = mconcat result
        in
          if not $ R.null ut
            then modify $ \(w', dwtt, bttt) -> (w', addDw (DW (l', l) $ S.fromList ut) dwtt, bttt)
            else pure ()
      else pure ()
  finish ww1


actor3 :: Edge
       -> ReadChannel (UpperVertex, LowerVertex)
       -> ReadChannel W
       -> ReadChannel Q
       -> ReadChannel BT
       -> ReadChannel W
       -> WriteChannel (UpperVertex, LowerVertex)
       -> WriteChannel W
       -> WriteChannel Q
       -> WriteChannel BT
       -> WriteChannel W
       -> StateT FilterState (DP st) ()
actor3 _ _ _ _ _ rfb _ _ _ _ wfb = do
  (_, dwtt, _) <- get
  unless (hasNotDW dwtt) $ do
    let (DWTT dtlist) = dwtt
    foldM_ rfb $ \w@(W l' w_t') -> do
      push w wfb
      forM_ dtlist $ \(DW (l_l, l_u) ut) ->
        let result =
              [ (u_1, u_2, u_3)
              | (u_1, u_2, u_3) <- S.toList ut
              , IS.member u_1 w_t' && IS.member u_3 w_t'
              , l' /= l_l && l' /= l_u
              ]
        in  unless (R.null result) $ modify $ \(w', dwtt', bttt) ->
              (w', dwtt', addBt (BT (l_l, l', l_u) $ S.fromList result) bttt)
  rfb |=>| wfb $ id
  finish wfb


actor4 :: Edge
       -> ReadChannel (UpperVertex, LowerVertex)
       -> ReadChannel W
       -> ReadChannel Q
       -> ReadChannel BT
       -> ReadChannel W
       -> WriteChannel (UpperVertex, LowerVertex)
       -> WriteChannel W
       -> WriteChannel Q
       -> WriteChannel BT
       -> WriteChannel W
       -> StateT FilterState (DP st) ()
actor4 _ _ _ query rbt _ _ _ wq wbt _ = do
  (_, _, bttt) <- get
  foldM_ query $ \e -> do
    push e wq
    unless (hasNotBT bttt) $ case e of
      ByVertex k ->
        if k `IS.member` _btttKeys bttt then forM_ (_btttBts bttt) (`push` wbt) else pure ()
      _ -> pure ()
  finish wq
  rbt |=>| wbt $ id


program :: FilePath -> IO ()
program file = runDP $ mkDP @DPBT (source' file) generator' sink'



