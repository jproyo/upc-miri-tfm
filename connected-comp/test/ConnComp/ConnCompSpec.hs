module ConnComp.ConnCompSpec
  ( spec
  ) where

import           ConnComp                                          as CC
import           Data.ConnCompFixture
import           Relude
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

spec :: Spec
spec = describe "Prestablished examples" $ do
  context "Examples that have been provided" $ do
    it "Example 1 CC - 1 Filter" $ do
      let input = "1 2\n 2 3\n 3 4\n 4 5\n 5 6\n"
      result <- liftIO $ runParallelWithExample input
      length result `shouldBe` 1
    it "Example 1 CC - 3 Filter" $ do
      let input = "1 2\n 3 4\n 5 6\n 2 3\n 4 5\n"
      result <- liftIO $ runParallelWithExample input
      length result `shouldBe` 1
    it "Example 2 CC - 2 Filter" $ do
      let input = "1 2\n 2 3\n 3 4\n 4 5\n 5 6\n 7 8\n 8 9\n 9 10\n"
      result <- liftIO $ runParallelWithExample input
      length result `shouldBe` 2
    it "Example 1 CC - 2 Filter" $ do
      let input = "1 2\n 1 3\n 1 4\n 1 5\n 5 2\n 5 3\n"
      result <- liftIO $ runParallelWithExample input
      length result `shouldBe` 1
    it "Example 3 CC" $ do
      let input = "1 2\n 2 3\n 1 3\n 3 5\n 4 5\n 1 4\n 7 8\n 3 6\n 7 9\n 11 10\n 10 12\n"
      result <- liftIO $ runParallelWithExample input
      length result `shouldBe` 3
    it "Example 3 CC - Shuffle" $ do
      let input = "1 2\n 2 3\n 4 5\n 1 4\n 1 3\n 7 8\n 10 12\n 3 5\n 3 6\n 7 9\n 11 10\n"
      result <- liftIO $ runParallelWithExample input
      length result `shouldBe` 3
  context "Property Based Testing Examples"
    $ modifyMaxSuccess (const 1000)
    $ it "Retrieves the correct number of connected components"
    $ property
    $ \Graph{..} -> do 
      result <- liftIO $ runParallelWithExample $ toEdgesByteString _gEdges
      length result `shouldBe` _gAmountConnComp


runParallelWithExample :: ByteString -> IO [ConnectedComponents Integer]
runParallelWithExample s = CC.fromByteString s >>= CC.runParallelDP'
