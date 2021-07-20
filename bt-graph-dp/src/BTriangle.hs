-- |
-- Module      : BTriangle
-- Copyright   : (c) 2021 Juan Pablo Royo Sales
--
-- License     : BSD3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : experimental
-- Portability : GHC
module BTriangle where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Data.IntSet                                       as IS
import           Data.Set                                          as S
import           Data.Time.Clock.POSIX
import           DynamicPipeline
import           Edges
import           Relude                                            as R
import           Numeric

t :: Integral b => POSIXTime -> IO b
t fct = round . (fct *) <$> getPOSIXTime

nanoSecs :: IO Double
nanoSecs = (/ 1000000) . fromInteger <$> t 1000000000

microSecs :: IO Double
microSecs = (/ 1000) . fromInteger <$> t 1000000

milliSecs :: IO Double
milliSecs = fromInteger <$> t 1000

showFullPrecision :: Double -> Text
showFullPrecision = toText . flip (showFFloat Nothing) ""

-- printCC :: Experiment -> Int -> Double -> Double -> IO ()
-- printCC e c now now2 = putLBSLn $ "test-1,"<> e <>"," <> show c <> "," <> showFullPrecision (now2 - now)

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
             -> IO ()
             )
source' = withSource @DPBT . toFilters

toFilters :: FilePath
          -> ReadChannel W
          -> WriteChannel (UpperVertex, LowerVertex)
          -> WriteChannel W
          -> WriteChannel Q
          -> WriteChannel BT
          -> WriteChannel W
          -> IO ()
toFilters filePath rfw wedges ww1 query wbt wfb = do
  unfoldFile filePath wedges (toEdge . decodeUtf8) -- Read from file and feed stream
  finish ww1 >> finish wbt -- mark as not used to continue in following filters
  rfw |=>| wfb $ id -- feedback channel
  liftIO doCommand
  finish query
 where
  doCommand = loop

  loop      = getLine >>= \l -> do
    now <- nanoSecs
    tid <- myThreadId
    putTextLn $ "New Command: " <> l <> " - Time: " <> showFullPrecision now <> " - Thread: " <> show tid
    case toCommand $ toString l of
      End -> pure ()
      c   -> push c query >> loop


sink' :: Stage (ReadChannel (UpperVertex, LowerVertex) -> ReadChannel W -> ReadChannel Q -> ReadChannel BT -> IO ())
sink' = withSink @DPBT toOutput

toOutput :: ReadChannel (UpperVertex, LowerVertex) -> ReadChannel W -> ReadChannel Q -> ReadChannel BT -> IO ()
toOutput _ _ _ rbt = foldM_ rbt $ \e -> liftIO $ do
  now <- nanoSecs
  putTextLn $ "Element Result: " <> show e <> " - Time: " <> showFullPrecision now

type FilterState = (W, DWTT, BTTT)

generator' :: forall k (st :: k) . GeneratorStage DPBT FilterState Edge
generator' = let gen = withGenerator @DPBT genAction in mkGenerator gen filterTemplate

genAction :: Filter DPBT FilterState Edge
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
          -> IO ()
genAction filter' redges rw1 rq rbt rfd _ _ _ wbt wfc = do
  let unfoldFilter = mkUnfoldFilterForAll filter'
                                          (\(u, l) -> (W l $ IS.singleton u, mempty, mempty))
                                          redges
                                          (rw1 .*. rq .*. rbt .*. rfd .*. HNil)
  HCons rw1' (HCons _ (HCons rbt' _)) <- unfoldF unfoldFilter
  rw1' |=>| wfc $ id
  rbt' |=>| wbt $ id

filterTemplate :: Filter DPBT FilterState Edge
filterTemplate = actor actor1 |>>> actor actor2 |>>> actor actor3 |>> actor actor4

actor1 :: IORef FilterState
       -> Edge
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
       -> IO ()
actor1 ref (_, l) redges _ _ _ _ we ww1 _ _ _ = do
  foldM_ redges $ \e@(u', l') -> do
    putTextLn $ "Reading edge" <> show e
    if l' == l
      then modifyIORef ref $ \(w@W {..}, dwtt, bttt) -> (w { _wWedges = addWedge _wWedges u' }, dwtt, bttt)
      else push e we
  finish we
  (w@(W _ w_t), _, _) <- readIORef ref
  when (IS.size w_t > 1) $ push w ww1

actor2 :: IORef FilterState
       -> Edge
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
       -> IO ()
actor2 ref (_, l) _ rw1 _ _ _ _ ww1 _ _ _ = do
  (W _ w_t, _, _) <- readIORef ref
  foldM_ rw1 $ \w@(W l' w_t') -> do
    putTextLn $ "[A2] - Reading rw1: " <> show w_t'
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
            then modifyIORef ref $ \(w', dwtt, bttt) -> (w', addDw (DW (l', l) $ S.fromList ut) dwtt, bttt)
            else pure ()
      else pure ()
  finish ww1


actor3 :: IORef FilterState
       -> Edge
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
       -> IO ()
actor3 ref e _ _ _ _ rfb _ _ _ _ wfb = do
  putTextLn $ "Starting building BT- Edge: " <> show e
  (_, dwtt, _) <- readIORef ref
  foldM_ rfb $ \w@(W l' w_t') -> do
    push w wfb
    unless (hasNotDW dwtt) $ do
      let (DWTT dtlist) = dwtt
      putTextLn $ "[A3] - Reading rfb: " <> show w <> " - Edge: " <> show e
      forM_ dtlist $ \(DW (l_l, l_u) ut) ->
        let result =
              [ (u_1, u_2, u_3)
              | (u_1, u_2, u_3) <- S.toList ut
              , IS.member u_1 w_t' && IS.member u_3 w_t'
              , l' /= l_l && l' /= l_u
              ]
        in  if not $ R.null result
              then modifyIORef ref
                $ \(w', dwtt', bttt) -> (w', dwtt', addBt (BT (l_l, l', l_u) $ S.fromList result) bttt)
              else pure ()
  putTextLn $ "Before Finishing building BT- Edge: " <> show e
  finish wfb
  putTextLn $ "Finishing building BT- Edge: " <> show e


actor4 :: IORef FilterState
       -> Edge
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
       -> IO ()
actor4 ref _ _ _ query rbt _ _ _ wq wbt _ = do
  (_, _, bttt) <- readIORef ref
  rbt |=> wbt
  foldM_ query $ \e -> do
    push e wq
    unless (hasNotBT bttt) $ case e of
      ByVertex k -> do
        liftIO $ do
          tid <- myThreadId
          putTextLn $ "New Command: " <> show k <> " - threadid: " <> show tid
        if not $ IS.null (IS.fromList k `IS.intersection` _btttKeys bttt)
          then forM_ (_btttBts bttt) (`push` wbt)
          else pure ()
      _ -> pure ()
  finish wq



program :: FilePath -> IO ()
program file = runDP $ mkDP @DPBT (source' file) generator' sink'



