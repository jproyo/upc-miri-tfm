module Main where

import           Criterion.Main
import           Criterion.Main.Options
import           Criterion.Types
import           Data.Time.Clock.POSIX
import           ConnComp
import           Relude
import           System.Directory


timeS :: IO [Char]
timeS = show . round @_ @Integer <$> getPOSIXTime

runBench :: IO ()
runBench = do
  time' <- timeS
  createDirectoryIfMissing True "output/"
  let conf = defaultConfig { reportFile = Just $ "results/report_benchmark_" <> time' <> ".html" }
  runMode (Run conf Glob ["*/*"])
    [ bgroup "connectedComponents - Data.Graph on containers"
       [ bench "ca-AstroPh" $ whnfIO (calculate "data/ca-AstroPh.txt")
       , bench "email-Enron" $ whnfIO (calculate "data/email-Enron.txt")
       , bench "web-Google" $ whnfIO (calculate "data/web-Google.txt")
       ],
       bgroup "connectedComponents - DP in Haskell"
       [ bench "ca-AstroPh" $ whnfIO (runDPConnectedComp "data/ca-AstroPh.txt")
       , bench "email-Enron" $ whnfIO (runDPConnectedComp "data/email-Enron.txt")
       , bench "web-Google" $ whnfIO (runDPConnectedComp "data/web-Google.txt")
       ]
    ]


main :: IO ()
main = runBench
