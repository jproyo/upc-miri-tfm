-- |
-- Module      : BTriangle
-- Copyright   : (c) 2021 Juan Pablo Royo Sales
--
-- License     : BSD3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : experimental
-- Portability : GHC
module BTriangle where

--import           Control.Concurrent
import           Control.Concurrent.Async
import           Data.IntSet                                       as IS
import           Data.Set                                          as S
import           DynamicPipeline
import           Edges
import           Relude                                            as R
import           System.Environment


-- brittany-disable-next-binding
type DPBT = Source (Channel (Edge :<+> W :<+> Q :<+> BT :<+> BTResult :<+> W :<+> Eof))
                :=> Generator (Channel (Edge :<+> W :<+> Q :<+> BT :<+> BTResult :<+> Eof))
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
             -> WriteChannel BTResult
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
          -> WriteChannel BTResult
          -> WriteChannel W
          -> DP st ()
toFilters filePath rfw wedges ww1 query wbt wrbt wfb = do
  unfoldFile filePath wedges (toEdge . decodeUtf8) -- Read from file and feed stream
  finish ww1 >> finish wbt >> finish wrbt -- mark as not used to continue in following filters
  rfw |=>| wfb $ id -- feedback channel
  liftIO printHeader
  now <- liftIO nanoSecs 
  liftIO $ doCommand now
  finish query
 where
  doCommand n = loop n

  loop n     = getLine >>= \l -> do
    case toCommand $ toString l of
      End       -> pure ()
      NoCommand -> putTextLn ("[ERROR] No Command was inputed. Possible commands: \n" <> commandsText) >> loop n
      c         -> push (Q c n) query >> loop n


sink' :: Stage
           (  ReadChannel (UpperVertex, LowerVertex)
           -> ReadChannel W
           -> ReadChannel Q
           -> ReadChannel BT
           -> ReadChannel BTResult
           -> DP s ()
           )
sink' = withSink @DPBT toOutput

toOutput :: ReadChannel (UpperVertex, LowerVertex)
         -> ReadChannel W
         -> ReadChannel Q
         -> ReadChannel BT
         -> ReadChannel BTResult
         -> DP s ()
toOutput _ _ _ _ rbt = do
  count <- newIORef 1
  foldM_ rbt $ \result -> liftIO $ do
    c <- readIORef count
    printCC "Test-1" result c
    modifyIORef count (+ 1)

type FilterState = (W, DWTT, BTTT)

generator' :: forall k (st :: k) . GeneratorStage DPBT FilterState Edge st
generator' = let gen = withGenerator @DPBT genAction in mkGenerator gen filterTemplate

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
                                          (\(u, l) -> (W l $ IS.singleton u, mempty, mempty))
                                          redges
                                          (rw1 .*. rq .*. rbt .*. rbtr .*. rfd .*. HNil)
  HCons rw1' (HCons _ (HCons _ (HCons rbtr' _))) <- unfoldF unfoldFilter
  rw1' |=>| wfc $ id
  rbtr' |=>| wbtr $ id

filterTemplate :: forall s . Filter DPBT FilterState Edge s
filterTemplate = actor actor1 |>>> actor actor2 |>>> actor actor3 |>> actor actor4

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
    if l' == l
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
  (W _ w_t, _, _) <- get
  foldM_ rw1 $ \w@(W l' w_t') -> do
    push w ww1
    buildDW w_t w_t' l l'
  finish ww1

buildDW :: IntSet -> IntSet -> LowerVertex -> LowerVertex -> StateT FilterState (DP st) ()
buildDW w_t w_t' l l' =
  let pair       = (min l l', max l l')
      paramBuild = if l < l' then (w_t, w_t') else (w_t', w_t)
      ut         = uncurry buildDW' paramBuild
  in  if (IS.size w_t > 1) && abs (l - l') > 1 && not (IS.null (IS.intersection w_t w_t')) && not (S.null ut)
        then modify $ \(w', dwtt, bttt) -> (w', addDw (DW pair ut) dwtt, bttt)
        else pure ()

buildDW' :: IntSet -> IntSet -> UT
buildDW' w_t w_t' =
  let si = w_t IS.\\ w_t'
      sj = IS.intersection w_t w_t'
      sk = w_t' IS.\\ w_t
      buildUt si' sj' sk' =
        [ (i, j, k) | i <- IS.toList si', j <- IS.toList sj', i /= j, k <- IS.toList sk', i /= k && j /= k ]
      cond_a = IS.size si >= 1 && IS.size sj > 0 && IS.size sk > 0
      cond_b = IS.size si == 0 && IS.size sj > 1 && IS.size sk > 0
      cond_c = IS.size si == 0 && IS.size sj > 2 && IS.size sk == 0
      cond_d = IS.size si > 0 && IS.size sj > 1 && IS.size sk == 0
      ut | cond_a    = buildUt si sj sk
         | cond_b    = buildUt sj sj sk
         | cond_c    = buildUt sj sj sj
         | cond_d    = buildUt si sj sj
         | otherwise = []
  in  S.fromList ut

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
  (_, dwtt, _) <- get
  foldM_ rfb $ \w@(W l' w_t') -> do
    push w wfb
    when (hasDW dwtt) $ do
      let (DWTT dtlist) = dwtt
      forM_ dtlist $ \(DW (l_l, l_u) ut) ->
        let triple = (l_l, l', l_u)
            result =
              [ (u_1, u_2, u_3)
              | l' < l_u && l' > l_l
              , (u_1, u_2, u_3) <- S.toList ut
              , u_1 `IS.member` w_t' && u_3 `IS.member` w_t'
              ]
        in  if not $ R.null result
              then modify $ \(w', dwtt'', bttt) -> (w', dwtt'', addBt (BT triple $ S.fromList result) bttt)
              else pure ()
  finish wfb
  whenM (liftIO $ lookupEnv "LOG_DEBUG" <&> isJust)
    $ putTextLn ("Finishing building BT for Filter with Param l=" <> show l)


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
  (_, _, bttt) <- get
  void $ liftIO $ async (rbtr |=> wbtr)
  foldM_ query $ \e@(Q q _) -> do
    push e wq
    unless (hasNotBT bttt) $ case q of
      ByVertex k | not $ IS.null (IS.fromList k `IS.intersection` _btttKeys bttt)     -> sendBts bttt e wbtr
      ByEdge edges | not $ S.null (S.fromList edges `S.intersection` _btttEdges bttt) -> sendBts bttt e wbtr
      AllBT                                                                           -> sendBts bttt e wbtr
      Count                                                                           -> do
        push (RC e (getSum $ R.foldMap (Sum . S.size . _btUpper) $ _btttBts bttt)) wbtr
      _ -> pure ()
  finish wq

sendBts :: MonadIO m => BTTT -> Q -> WriteChannel BTResult -> m ()
sendBts bttt q wbtr = forM_ (_btttBts bttt) (flip push wbtr . RBT q)

program :: FilePath -> IO ()
program file = runDP $ mkDP @DPBT (source' file) generator' sink'



