module Utils.Trace where

import           Data.Time.Clock.POSIX          ( POSIXTime
                                                , getPOSIXTime
                                                )
--import           Numeric
import           Relude

type Experiment = Text 

t :: Integral b => POSIXTime -> IO b
t fct = round . (fct *) <$> getPOSIXTime

nanoSecs :: IO Integer
nanoSecs = fromInteger <$> t 1000000000

microSecs :: IO Integer
microSecs = fromInteger <$> t 1000000

milliSecs :: IO Integer
milliSecs = fromInteger <$> t 1000

showFullPrecision :: Integer -> Text
showFullPrecision = show

takeTime :: IO Integer
takeTime = microSecs

printCC :: Experiment -> Int -> Integer -> Integer -> IO ()
printCC e c now now2 = putTextLn $ "test-1,"<> e <>"," <> show c <> "," <> showFullPrecision (now2 - now)


