-- |
-- Module      : BTriangle
-- Copyright   : (c) 2021 Juan Pablo Royo Sales
--
-- License     : BSD3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : experimental
-- Portability : GHC
module BTriangle where

import           Data.IntSet                                       as IS
import           DynamicPipeline
import           Edges
import           Relude                                            as R
import           System.Environment                                                                                   ( lookupEnv
                                                                                                                      )


-- brittany-disable-next-binding
type DPBT = Source (Channel (Edge :<+> W :<+> Q :<+> BT :<+> BTResult :<+> W :<+> Eof))
                :=> Generator (Channel (Edge :<+> W :<+> Q :<+> BT :<+> BTResult :<+> Eof))
                :=> FeedbackChannel (W :<+> Eof)
                :=> Sink

source' :: forall k (st :: k)
         . Conf
        -> Stage
             (  ReadChannel W
             -> WriteChannel (UpperVertex, LowerVertex)
             -> WriteChannel W
             -> WriteChannel Q
             -> WriteChannel BT
             -> WriteChannel BTResult
             -> WriteChannel W
             -> DP st ()
             )
source' = withSource @DPBT . toFilters

{-# INLINE  toFilters #-}
toFilters :: Conf
          -> ReadChannel W
          -> WriteChannel (UpperVertex, LowerVertex)
          -> WriteChannel W
          -> WriteChannel Q
          -> WriteChannel BT
          -> WriteChannel BTResult
          -> WriteChannel W
          -> DP st ()
toFilters Conf {..} rfw wedges ww1 query wbt wrbt wfb = do
  unfoldFile _edgeFile wedges (toEdge . decodeUtf8) -- Read from file and feed stream
  finish ww1 >> finish wbt >> finish wrbt -- mark as not used to continue in following filters
  rfw |=>| wfb $ id -- feedback channel
  liftIO printHeader
  now <- liftIO nanoSecs
  unfoldFile _commandFile query (\c -> Q (toCommand . decodeUtf8 $ c) now _experimentName) 

sink' :: Stage
           (  ReadChannel (UpperVertex, LowerVertex)
           -> ReadChannel W
           -> ReadChannel Q
           -> ReadChannel BT
           -> ReadChannel BTResult
           -> DP s ()
           )
sink' = withSink @DPBT toOutput

{-# INLINE  toOutput #-}
toOutput :: ReadChannel (UpperVertex, LowerVertex)
         -> ReadChannel W
         -> ReadChannel Q
         -> ReadChannel BT
         -> ReadChannel BTResult
         -> DP s ()
toOutput _ _ _ _ rbt = do
  c <- newIORef 1
  foldM_ rbt $ \result -> liftIO $ readIORef c >>= printCC result >> modifyIORef c (+ 1)

type FilterState = (W, Either DWTT BTTT)

generator' :: forall k (st :: k) . GeneratorStage DPBT FilterState Edge st
generator' = let gen = withGenerator @DPBT genAction in mkGenerator gen filterTemplate

{-# INLINE  genAction #-}
genAction :: forall s
           . Filter DPBT FilterState Edge s
          -> ReadChannel (UpperVertex, LowerVertex)
          -> ReadChannel W
          -> ReadChannel Q
          -> ReadChannel BT
          -> ReadChannel BTResult
          -> ReadChannel W
          -> WriteChannel (UpperVertex, LowerVertex)
          -> WriteChannel W
          -> WriteChannel Q
          -> WriteChannel BT
          -> WriteChannel BTResult
          -> WriteChannel W
          -> DP s ()
genAction filter' redges rw1 rq rbt rbtr rfd _ _ _ _ wbtr wfc = do
  let unfoldFilter = mkUnfoldFilterForAll filter'
                                          (\(u, l) -> (W l $ IS.singleton u, Left mempty))
                                          redges
                                          (rw1 .*. rq .*. rbt .*. rbtr .*. rfd .*. HNil)
  HCons rw1' (HCons _ (HCons _ (HCons rbtr' _))) <- unfoldF unfoldFilter
  rw1' |=>| wfc $ id
  rbtr' |=>| wbtr $ id

{-# INLINE  filterTemplate #-}
filterTemplate :: forall s . Filter DPBT FilterState Edge s
filterTemplate = actor actor1 |>>> actor actor2 |>>> actor actor3 |>> actor actor4

{-# INLINE  actor1 #-}
actor1 :: Edge
       -> ReadChannel (UpperVertex, LowerVertex)
       -> ReadChannel W
       -> ReadChannel Q
       -> ReadChannel BT
       -> ReadChannel BTResult
       -> ReadChannel W
       -> WriteChannel (UpperVertex, LowerVertex)
       -> WriteChannel W
       -> WriteChannel Q
       -> WriteChannel BT
       -> WriteChannel BTResult
       -> WriteChannel W
       -> StateT FilterState (DP st) ()
actor1 (_, l) redges _ _ _ _ _ we ww1 _ _ _ _ = do
  foldM_ redges $ \e@(u', l') -> do
    e `seq` if l' == l
      then modify $ \(w@W {..}, dwtt) -> (w { _wWedges = _wWedges `seq` addWedge _wWedges u' }, dwtt)
      else push e we
  finish we
  (w@(W _ w_t), _) <- get
  when (IS.size w_t > 1) $ push w ww1

{-# INLINE  actor2 #-}
actor2 :: Edge
       -> ReadChannel (UpperVertex, LowerVertex)
       -> ReadChannel W
       -> ReadChannel Q
       -> ReadChannel BT
       -> ReadChannel BTResult
       -> ReadChannel W
       -> WriteChannel (UpperVertex, LowerVertex)
       -> WriteChannel W
       -> WriteChannel Q
       -> WriteChannel BT
       -> WriteChannel BTResult
       -> WriteChannel W
       -> StateT FilterState (DP st) ()
actor2 (_, l) _ rw1 _ _ _ _ _ ww1 _ _ _ _ = do
  (W _ w_t, _) <- get
  foldM_ rw1 $ \w@(W l' w_t') -> do
    push w ww1
    buildDW w_t w_t' l l'
  finish ww1

{-# INLINE  buildDW #-}
buildDW :: IntSet -> IntSet -> LowerVertex -> LowerVertex -> StateT FilterState (DP st) ()
buildDW !w_t !w_t' l l' =
  let pair       = (min l l', max l l')
      paramBuild = if l < l' then (w_t, w_t') else (w_t', w_t)
      ut         = uncurry buildDW' paramBuild
  in  if (IS.size w_t > 1) && abs (l - l') > 1 && not (IS.null (IS.intersection w_t w_t')) && not (R.null ut)
        then modify $ second (first (addDw (DW pair ut)))
        else pure ()

{-# INLINE  buildDW' #-}
buildDW' :: IntSet -> IntSet -> UT
buildDW' !w_t !w_t' =
  let !si = w_t IS.\\ w_t'
      !sj = IS.intersection w_t w_t'
      !sk = w_t' IS.\\ w_t
      buildUt !si' !sj' !sk' =
        [ (i, j, k) | i <- IS.toList si', j <- IS.toList sj', i /= j, k <- IS.toList sk', i /= k && j /= k ]
      !cond_a = IS.size si >= 1 && IS.size sj > 0 && IS.size sk > 0
      !cond_b = IS.size si == 0 && IS.size sj > 1 && IS.size sk > 0
      !cond_c = IS.size si == 0 && IS.size sj > 2 && IS.size sk == 0
      !cond_d = IS.size si > 0 && IS.size sj > 1 && IS.size sk == 0
      ut | cond_a    = buildUt si sj sk
         | cond_b    = buildUt sj sj sk
         | cond_c    = buildUt sj sj sj
         | cond_d    = buildUt si sj sj
         | otherwise = []
  in ut

{-# INLINE  actor3 #-}
actor3 :: Edge
       -> ReadChannel (UpperVertex, LowerVertex)
       -> ReadChannel W
       -> ReadChannel Q
       -> ReadChannel BT
       -> ReadChannel BTResult
       -> ReadChannel W
       -> WriteChannel (UpperVertex, LowerVertex)
       -> WriteChannel W
       -> WriteChannel Q
       -> WriteChannel BT
       -> WriteChannel BTResult
       -> WriteChannel W
       -> StateT FilterState (DP st) ()
actor3 (_, l) _ _ _ _ _ rfb _ _ _ _ _ wfb = do
  (_, dwtt') <- get
  modify $ second (const $ Right mempty)
  foldM_ rfb $ \w@(W l' w_t') -> do
    push w wfb
    case dwtt' of 
      Left dwtt -> 
        when (hasDW dwtt) $ do
          let (DWTT dtlist) = dwtt
          forM_ dtlist $ \(DW (l_l, l_u) ut) ->
            let triple = (l_l, l', l_u)
                result =
                  [ (u_1, u_2, u_3)
                  | l' < l_u && l' > l_l
                  , (u_1, u_2, u_3) <- ut
                  , u_1 `IS.member` w_t' && u_3 `IS.member` w_t'
                  ]
            in  if not $ R.null result
                  then modify $ second (second (addBt (BT triple result)))
                  else pure ()
      Right _ -> pure ()
  finish wfb
  whenM (liftIO $ lookupEnv "LOG_DEBUG" <&> isJust)
    $ putTextLn ("Finishing building BT for Filter with Param l=" <> show l)

{-# INLINE  actor4 #-}
actor4 :: Edge
       -> ReadChannel (UpperVertex, LowerVertex)
       -> ReadChannel W
       -> ReadChannel Q
       -> ReadChannel BT
       -> ReadChannel BTResult
       -> ReadChannel W
       -> WriteChannel (UpperVertex, LowerVertex)
       -> WriteChannel W
       -> WriteChannel Q
       -> WriteChannel BT
       -> WriteChannel BTResult
       -> WriteChannel W
       -> StateT FilterState (DP st) ()
actor4 _ _ _ query _ rbtr _ _ _ wq _ wbtr _ = do
  (_, bttt') <- get
  rbtr |=> wbtr
  foldM_ query $ \e@(Q q _ _) -> do
    push e wq
    case bttt' of
      Left _ -> pure ()
      Right bttt ->
        unless (hasNotBT bttt) $ case q of
          ByVertex k | containsVertex k bttt      -> sendBts bttt e wbtr
          ByEdge edges | containsEdges edges bttt -> sendBts bttt e wbtr
          AllBT                                                                           -> sendBts bttt e wbtr
          Count                                                                           ->
            push (RC e (getSum $ R.foldMap (Sum . R.length . _btUpper) $ _btttBts bttt)) wbtr
          _ -> pure ()

{-# INLINE  sendBts #-}
sendBts :: MonadIO m => BTTT -> Q -> WriteChannel BTResult -> m ()
sendBts !bttt !q wbtr = forM_ (_btttBts bttt) (flip push wbtr . RBT q)

program :: Conf -> IO ()
program conf = runDP $ mkDP @DPBT (source' conf) generator' sink'



