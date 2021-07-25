module Main where

import           BTriangle
import           Control.Monad
import           Control.Monad.ST
import           Data.Array.ST
import           Data.STRef
import           Edges
import           GHC.IO.Handle                                     as H
import           Relude                                            as R
import           System.Environment
import           System.Random

main :: IO ()
main = do
 file <- maybe (fail "Error no parameter found") return . R.viaNonEmpty R.head =<< getArgs
 program file


some' :: IO ()
some' = do
  file   <- maybe (fail "Error no parameter found") return . R.viaNonEmpty R.head =<< getArgs
  edges  <- R.withFile file R.ReadMode $ \h -> accumEdge h
  gen    <- getStdGen
  (result, _) <- R.foldlM (\(acc, g) _ -> let (a, b) = shuffle' edges g in pure (a : acc, b)) ([], gen) [1 .. 10]
  forM_ (zip [10 ..] result) $ \(i, r) -> R.withFile ("./input/test_" <> show i <> ".txt") R.WriteMode $ \h -> forM_ r $ \(a,b) -> H.hPutStr h (show a <> " " <> show b <> "\n")

accumEdge :: R.Handle -> IO [Edge]
accumEdge h = loop h [] where loop h l= ifM (H.hIsEOF h) (pure l) (H.hGetLine h >>= loop h . (: l) . toEdge)


-- | Randomly shuffle a list without the IO Monad
--   /O(N)/
shuffle' :: [a] -> StdGen -> ([a], StdGen)
shuffle' xs gen = runST
  (do
    g <- newSTRef gen
    let randomRST lohi = do
          (a, s') <- liftM (randomR lohi) (readSTRef g)
          writeSTRef g s'
          return a
    ar  <- newArray n xs
    xs' <- forM [1 .. n] $ \i -> do
      j  <- randomRST (i, n)
      vi <- readArray ar i
      vj <- readArray ar j
      writeArray ar j vi
      return vj
    gen' <- readSTRef g
    return (xs', gen')
  )
 where
  n = length xs
  newArray :: Int -> [a] -> ST s (STArray s Int a)
  newArray n xs = newListArray (1, n) xs
