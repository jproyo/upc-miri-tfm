module Main where

import           BTriangle
import           Criterion.Main
import           Criterion.Main.Options
import           Criterion.Types
import           Data.Time.Clock.POSIX
import           Edges
import           Relude
import           System.Directory


timeS :: IO [Char]
timeS = show . round @_ @Integer <$> getPOSIXTime

runBench :: IO ()
runBench = do
  time' <- timeS
  createDirectoryIfMissing True "benchmark/output/"
  let conf = defaultConfig { reportFile = Just $ "benchmark/output/report_benchmark_" <> time' <> ".html" }
  runMode
    (Run conf Glob ["*/*"])
    [ bgroup
      "BiTriangle Enumeration - Search by-vertex"
      [ bench "opsahl-ucforum" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/opsahl-ucforum/input.txt"
                        , _commandFile    = "benchmark/opsahl-ucforum/c-vertex.txt"
                        , _experimentName = "opsahl-ucforum-by-vertex"
                        }
        )
      , bench "wang-amazon" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/wang-amazon/input.txt"
                        , _commandFile    = "benchmark/wang-amazon/c-vertex.txt"
                        , _experimentName = "wang-amazon-by-vertex"
                        }
        )
      , bench "moreno_crime" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/moreno_crime/input.txt"
                        , _commandFile    = "benchmark/moreno_crime/c-vertex.txt"
                        , _experimentName = "moreno_crime-by-vertex"
                        }
        )
      ]
    , bgroup
      "BiTriangle Enumeration - Search by-edge"
      [ bench "opsahl-ucforum" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/opsahl-ucforum/input.txt"
                        , _commandFile    = "benchmark/opsahl-ucforum/c-edge.txt"
                        , _experimentName = "opsahl-ucforum-by-edge"
                        }
        )
      , bench "wang-amazon" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/wang-amazon/input.txt"
                        , _commandFile    = "benchmark/wang-amazon/c-edge.txt"
                        , _experimentName = "wang-amazon-by-edge"
                        }
        )
      , bench "moreno_crime" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/moreno_crime/input.txt"
                        , _commandFile    = "benchmark/moreno_crime/c-edge.txt"
                        , _experimentName = "moreno_crime-by-edge"
                        }
        )
      ]
    , bgroup
      "BiTriangle Enumeration - Search by-both"
      [ bench "opsahl-ucforum" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/opsahl-ucforum/input.txt"
                        , _commandFile    = "benchmark/opsahl-ucforum/c-both.txt"
                        , _experimentName = "opsahl-ucforum-by-both"
                        }
        )
      , bench "wang-amazon" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/wang-amazon/input.txt"
                        , _commandFile    = "benchmark/wang-amazon/c-both.txt"
                        , _experimentName = "wang-amazon-by-both"
                        }
        )
      , bench "moreno_crime" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/moreno_crime/input.txt"
                        , _commandFile    = "benchmark/moreno_crime/c-both.txt"
                        , _experimentName = "moreno_crime-by-both"
                        }
        )
      ]
    ]


main :: IO ()
main = runBench
