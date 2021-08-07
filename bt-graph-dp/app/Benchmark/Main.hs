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
    [ expByLowerVertex, expByUpperVertex, expByEdge ]

expByLowerVertex :: Benchmark
expByLowerVertex = bgroup
      "BiTriangle Enumeration - Search by-vertex in Lower Layer"
      [ bench "opsahl-ucforum-vertex-lower-low" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/opsahl-ucforum/input.txt"
                        , _commandFile    = "benchmark/opsahl-ucforum/c-vertex-lower-low.txt"
                        , _experimentName = "opsahl-ucforum-vertex-lower-low"
                        }
        )
      , bench "opsahl-ucforum-vertex-lower-medium" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/opsahl-ucforum/input.txt"
                        , _commandFile    = "benchmark/opsahl-ucforum/c-vertex-lower-medium.txt"
                        , _experimentName = "opsahl-ucforum-vertex-lower-medium"
                        }
        )
      , bench "opsahl-ucforum-vertex-lower-high" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/opsahl-ucforum/input.txt"
                        , _commandFile    = "benchmark/opsahl-ucforum/c-vertex-lower-high.txt"
                        , _experimentName = "opsahl-ucforum-lower-high"
                        }
        )
      , bench "wang-amazon-vertex-lower-low" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/wang-amazon/input.txt"
                        , _commandFile    = "benchmark/wang-amazon/c-vertex-lower-low.txt"
                        , _experimentName = "wang-amazon-vertex-lower-low"
                        }
        )
      , bench "wang-amazon-vertex-lower-medium" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/wang-amazon/input.txt"
                        , _commandFile    = "benchmark/wang-amazon/c-vertex-lower-medium.txt"
                        , _experimentName = "wang-amazon-vertex-lower-medium"
                        }
        )
      , bench "wang-amazon-vertex-lower-high" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/wang-amazon/input.txt"
                        , _commandFile    = "benchmark/wang-amazon/c-vertex-lower-high.txt"
                        , _experimentName = "wang-amazon-vertex-lower-high"
                        }
        )
      , bench "moreno_crime-vertex-lower-low" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/moreno_crime/input.txt"
                        , _commandFile    = "benchmark/moreno_crime/c-vertex-lower-low.txt"
                        , _experimentName = "moreno_crime-vertex-lower-low"
                        }
        )
      , bench "moreno_crime-vertex-lower-medium" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/moreno_crime/input.txt"
                        , _commandFile    = "benchmark/moreno_crime/c-vertex-lower-medium.txt"
                        , _experimentName = "moreno_crime-vertex-lower-medium"
                        }
        )
      , bench "moreno_crime-vertex-lower-high" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/moreno_crime/input.txt"
                        , _commandFile    = "benchmark/moreno_crime/c-vertex-lower-high.txt"
                        , _experimentName = "moreno_crime-vertex-lower-high"
                        }
        )
      ]

expByUpperVertex :: Benchmark
expByUpperVertex = bgroup
      "BiTriangle Enumeration - Search by-vertex in Upper Layer"
      [ bench "opsahl-ucforum-vertex-upper-low" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/opsahl-ucforum/input.txt"
                        , _commandFile    = "benchmark/opsahl-ucforum/c-vertex-upper-low.txt"
                        , _experimentName = "opsahl-ucforum-vertex-upper-low"
                        }
        )
      , bench "opsahl-ucforum-vertex-upper-medium" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/opsahl-ucforum/input.txt"
                        , _commandFile    = "benchmark/opsahl-ucforum/c-vertex-upper-medium.txt"
                        , _experimentName = "opsahl-ucforum-vertex-upper-medium"
                        }
        )
      , bench "opsahl-ucforum-vertex-upper-high" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/opsahl-ucforum/input.txt"
                        , _commandFile    = "benchmark/opsahl-ucforum/c-vertex-upper-high.txt"
                        , _experimentName = "opsahl-ucforum-vertex-upper-high"
                        }
        )
      , bench "wang-amazon-vertex-upper-low" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/wang-amazon/input.txt"
                        , _commandFile    = "benchmark/wang-amazon/c-vertex-upper-low.txt"
                        , _experimentName = "wang-amazon-vertex-upper-low"
                        }
        )
      , bench "wang-amazon-vertex-upper-medium" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/wang-amazon/input.txt"
                        , _commandFile    = "benchmark/wang-amazon/c-vertex-upper-medium.txt"
                        , _experimentName = "wang-amazon-vertex-upper-medium"
                        }
        )
      , bench "wang-amazon-vertex-upper-high" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/wang-amazon/input.txt"
                        , _commandFile    = "benchmark/wang-amazon/c-vertex-upper-high.txt"
                        , _experimentName = "wang-amazon-vertex-upper-high"
                        }
        )
      , bench "moreno_crime-vertex-upper-low" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/moreno_crime/input.txt"
                        , _commandFile    = "benchmark/moreno_crime/c-vertex-upper-low.txt"
                        , _experimentName = "moreno_crime-vertex-upper-low"
                        }
        )
      , bench "moreno_crime-vertex-upper-medium" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/moreno_crime/input.txt"
                        , _commandFile    = "benchmark/moreno_crime/c-vertex-upper-medium.txt"
                        , _experimentName = "moreno_crime-vertex-upper-medium"
                        }
        )
      , bench "moreno_crime-vertex-upper-high" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/moreno_crime/input.txt"
                        , _commandFile    = "benchmark/moreno_crime/c-vertex-upper-high.txt"
                        , _experimentName = "moreno_crime-vertex-upper-high"
                        }
        )
      ]

expByEdge :: Benchmark
expByEdge = bgroup
      "BiTriangle Enumeration - Search by-edge"
      [ bench "opsahl-ucforum-edge-low" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/opsahl-ucforum/input.txt"
                        , _commandFile    = "benchmark/opsahl-ucforum/c-edge-low.txt"
                        , _experimentName = "opsahl-ucforum-edge-low"
                        }
        )
      , bench "opsahl-ucforum-edge-medium" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/opsahl-ucforum/input.txt"
                        , _commandFile    = "benchmark/opsahl-ucforum/c-edge-medium.txt"
                        , _experimentName = "opsahl-ucforum-edge-medium"
                        }
        )
      , bench "opsahl-ucforum-edge-high" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/opsahl-ucforum/input.txt"
                        , _commandFile    = "benchmark/opsahl-ucforum/c-edge-high.txt"
                        , _experimentName = "opsahl-ucforum-edge-high"
                        }
        )
      , bench "wang-amazon-edge-low" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/wang-amazon/input.txt"
                        , _commandFile    = "benchmark/wang-amazon/c-edge-low.txt"
                        , _experimentName = "wang-amazon-edge-low"
                        }
        )
      , bench "wang-amazon-edge-medium" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/wang-amazon/input.txt"
                        , _commandFile    = "benchmark/wang-amazon/c-edge-medium.txt"
                        , _experimentName = "wang-amazon-edge-medium"
                        }
        )
      , bench "wang-amazon-edge-high" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/wang-amazon/input.txt"
                        , _commandFile    = "benchmark/wang-amazon/c-edge-high.txt"
                        , _experimentName = "wang-amazon-edge-high"
                        }
        )
      , bench "moreno_crime-edge-low" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/moreno_crime/input.txt"
                        , _commandFile    = "benchmark/moreno_crime/c-edge-low.txt"
                        , _experimentName = "moreno_crime-edge-low"
                        }
        )
      , bench "moreno_crime-edge-medium" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/moreno_crime/input.txt"
                        , _commandFile    = "benchmark/moreno_crime/c-edge-medium.txt"
                        , _experimentName = "moreno_crime-edge-medium"
                        }
        )
      , bench "moreno_crime-edge-high" $ whnfIO
        (program $ Conf { _edgeFile       = "benchmark/moreno_crime/input.txt"
                        , _commandFile    = "benchmark/moreno_crime/c-edge-high.txt"
                        , _experimentName = "moreno_crime-edge-high"
                        }
        )
      ]


main :: IO ()
main = runBench
