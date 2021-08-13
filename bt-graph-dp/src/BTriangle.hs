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

{-# INLINE toFilters #-}
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

{-# INLINE toOutput #-}
toOutput :: ReadChannel (UpperVertex, LowerVertex)
         -> ReadChannel W
         -> ReadChannel Q
         -> ReadChannel BT
         -> ReadChannel BTResult
         -> DP s ()
toOutput _ _ _ _ rbt = do
  c  <- newIORef 1
  c2 <- newIORef (0 :: Integer)
  foldM_ rbt $ \case
    r@(RBT _ _)      -> liftIO $ readIORef c >>= printCC r >> modifyIORef c (+ 1)
    RC _      countR -> liftIO $ modifyIORef c2 (+ toInteger countR)
  readIORef c2 >>= \cr -> when (cr > 0) $ putTextLn $ "[BT-TOTAL] = " <> show cr

generator' :: forall k (st :: k) . GeneratorStage DPBT FilterState Edge st
generator' = let gen = withGenerator @DPBT genAction in mkGenerator gen filterTemplate

{-# INLINE genAction #-}
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
                                          (\(u, l) -> Adj $ W l (IS.singleton u))
                                          redges
                                          (rw1 .*. rq .*. rbt .*. rbtr .*. rfd .*. HNil)
  HCons rw1' (HCons _ (HCons _ (HCons rbtr' _))) <- unfoldF unfoldFilter
  rw1' |=>| wfc $ id
  rbtr' |=>| wbtr $ id

{-# INLINE filterTemplate #-}
filterTemplate :: forall s . Filter DPBT FilterState Edge s
filterTemplate = actor actor1 |>>> actor actor2 |>>> actor actor3 |>> actor actor4

{-# INLINE actor1 #-}
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
    e `seq` if l' == l then modify $ flip modifyWState u' else push e we
  finish we
  state' <- get
  case state' of
    Adj w@(W _ ws) -> when (IS.size ws > 1) $ push w ww1
    _              -> pure ()

{-# INLINE actor2 #-}
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
  state' <- get
  case state' of
    Adj (W _ w_t) -> do
      modify $ const $ DoubleWedges mempty
      now <- printDebug "DW" l Nothing
      foldM_ rw1 $ \w@(W l' w_t') -> do
        push w ww1
        buildDW w_t w_t' l l'
      finish ww1
      void $ printDebug "DW" l $ Just now
    _ -> pure ()


{-# INLINE buildDW #-}
buildDW :: IntSet -> IntSet -> LowerVertex -> LowerVertex -> StateT FilterState (DP st) ()
buildDW w_t w_t' l l' =
  let pair       = Pair (min l l') (max l l')
      paramBuild = if l < l' then (w_t, w_t') else (w_t', w_t)
      ut         = uncurry buildDW' paramBuild
  in  if (IS.size w_t > 1) && l /= l' && not (IS.null (IS.intersection w_t w_t')) && not (nullUT ut)
        then modify $ flip modifyDWState (DW pair ut)
        else pure ()



{-# INLINE buildDW' #-}
buildDW' :: IntSet -> IntSet -> UT
buildDW' !w_t !w_t' = (w_t IS.\\ w_t', IS.intersection w_t w_t', w_t' IS.\\ w_t)

{-# INLINE actor3 #-}
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
  state' <- get
  case state' of
    DoubleWedges dwtt -> do
      modify $ const $ BiTriangles mempty
      now <- printDebug "BT" l Nothing
      foldM_ rfb $ \w@(W l' w_t') -> do
        push w wfb
        when (hasDW dwtt) $ do
          let (DWTT dtlist) = dwtt
          forM_ dtlist $ \(DW (Pair l_l l_u) ut) ->
            let triple = Triplet l_l l' l_u
                result =
                  if l' < l_u && l' > l_l then filterUt w_t' ut else Nothing
            in  maybe (pure ()) (modify . flip modifyBTState . BT triple) result
      finish wfb
      void $ printDebug "BT" l $ Just now
    _ -> pure ()

{-# INLINE filterUt #-}
filterUt :: IntSet -> UT -> Maybe UT
filterUt wt (si, sj, sk) =
  let si' = IS.filter (`IS.member` wt) si 
      sj' = IS.filter (`IS.member` wt) sj
      sk' = IS.filter (`IS.member` wt) sk
      sij' = si' `IS.union` sj'
      sjk' = sk' `IS.union` sj'
      wtInSome = not (IS.null sij' || IS.null sjk')
  in  if wtInSome then Just (sij', sj, sjk') else Nothing


-- {-# INLINE inSomeLeftAndRight #-}
-- inSomeLeftAndRight :: IntSet -> UT -> Bool
-- inSomeLeftAndRight wt (si, sj, sk) =
--    not (IS.null (wt `IS.intersection` (si `IS.union` sj)) || IS.null (wt `IS.intersection` (sj `IS.union` sk)))

{-# INLINE actor4 #-}
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
actor4 (_, l) _ _ query _ rbtr _ _ _ wq _ wbtr _ = do
  state' <- get
  case state' of
    BiTriangles bttt -> do
      now <- printDebug "QUERY" l Nothing
      rbtr |=> wbtr
      foldM_ query $ \e -> do
        push e wq
        unless (hasNotBT bttt) $ sendBts bttt e wbtr
      void $ printDebug "QUERY" l $ Just now

    _ -> pure ()

{-# INLINE sendBts #-}
sendBts :: MonadIO m => BTTT -> Q -> WriteChannel BTResult -> m ()
sendBts (BTTT bttt) q@(Q c _ _) wbtr = case c of
  ByVertex vx    -> forM_ bttt (\bt -> filterBTByVertex bt vx (flip push wbtr . RBT q))
  ByEdge   edges -> forM_ bttt (\bt -> filterBTByEdge bt edges (flip push wbtr . RBT q))
  AllBT          -> forM_ bttt (R.mapM_ (flip push wbtr . RBT q) . buildBT)
  Count          -> forM_ bttt (flip push wbtr . RC q . R.length . buildBT)
  _              -> pure ()

{-# INLINE printDebug #-}
printDebug :: MonadIO m => Text -> Int -> Maybe Double -> m Double
printDebug step param diff = do
  envEnable <- liftIO $ lookupEnv "LOG_DEBUG" <&> isJust
  if envEnable
    then do
      now <- liftIO milliSecs
      let dfs = maybe "" (mappend " - Diff: " . toText . showFullPrecision . (now -)) diff
      let st  = maybe "Starting" (const "Finish") diff
      let msg = mconcat
            [ "["
            , step
            , "] - ["
            , st
            , "] - Filter with Param l="
            , show param
            , " - Time: "
            , toText $ showFullPrecision now
            , dfs
            ]
      liftIO $ putTextLn msg
      return now
    else return (0 :: Double)

program :: Conf -> IO ()
program conf = runDP $ mkDP @DPBT (source' conf) generator' sink'



