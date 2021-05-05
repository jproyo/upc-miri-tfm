module Utils.Trace where

import           Data.Time.Clock.POSIX          ( POSIXTime
                                                , getPOSIXTime
                                                )
import           Numeric
import           Relude

type Experiment = LByteString 

t :: Integral b => POSIXTime -> IO b
t fct = round . (fct *) <$> getPOSIXTime

nanoSecs :: IO Double
nanoSecs = (/ 1000000) . fromInteger <$> t 1000000000

microSecs :: IO Double
microSecs = (/ 1000) . fromInteger <$> t 1000000

milliSecs :: IO Double
milliSecs = fromInteger <$> t 1000

showFullPrecision :: Double -> LByteString
showFullPrecision = encodeUtf8 . flip (showFFloat Nothing) ""

printCC :: Experiment -> Int -> Double -> Double -> IO ()
printCC e c now now2 = putLBSLn $ "test-1,"<> e <>"," <> show c <> "," <> showFullPrecision (now2 - now)


