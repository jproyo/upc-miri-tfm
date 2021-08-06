module Main where

import           BTriangle
import           Control.Monad
import           Edges
import           Options.Applicative                               as Opt
import           Relude                                            as R
import System.IO

edgesFile :: Opt.Parser FilePath
edgesFile = Opt.strOption (Opt.long "edges" <> Opt.short 'f' <> Opt.metavar "FilePath" <> Opt.help "Edges file path")

commandFile :: Opt.Parser FilePath
commandFile =
  Opt.strOption (Opt.long "commands" <> Opt.short 'c' <> Opt.metavar "FilePath" <> Opt.help "Commands file path")

experimentName :: Opt.Parser Text
experimentName =
  Opt.strOption (Opt.long "experiment" <> Opt.short 'e' <> Opt.metavar "Text" <> Opt.help "Experiment Name")


conf :: Opt.ParserInfo Conf
conf = info
  (Conf <$> edgesFile <*> commandFile <*> experimentName <**> helper)
  (fullDesc <> progDesc "Bi-Triangles Enumeration with DP" <> header "bt-graph-dp - Bi-Triangles Enumeration with DP")

main :: IO ()
main = do 
  hSetBuffering stdout LineBuffering  
  hSetBuffering stderr LineBuffering 
  Opt.execParser conf >>= program

