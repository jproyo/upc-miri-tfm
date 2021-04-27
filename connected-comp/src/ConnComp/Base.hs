module ConnComp.Base
  ( calculate
  ) where

import           Data.ByteString                                   as B
import           Data.Graph                                        as G
import           Relude                                            as R
import           Text.Trifecta
import           Text.Trifecta.Parser                              as P



calculate :: FilePath -> IO ()
calculate file = do
  bs   <- B.readFile file
  let x = P.parseByteString (runStateT parseEdges (maxInt, minInt)) mempty bs
  case x of
    Success (es, b) -> do
      let g  = G.buildG b es
      let cc = components g
      putTextLn $ "Connected Components: " <> show (R.length $ R.filter (\(Node _ xs) -> not $ R.null xs) cc)
    Failure ex -> error $ "Parsing" <> show ex

type Bound = (Int, Int)

parseEdges :: StateT Bound Parser [Edge]
parseEdges = many parseEdge'

parseInt :: Parser Int
parseInt = fromInteger <$> integer

parseEdge' :: StateT Bound Parser Edge
parseEdge' = do
  edge <- lift $ (,) <$> (whiteSpace *> parseInt <* whiteSpace) <*> parseInt <* optional newline
  modify (updateBound edge)
  return edge


updateBound :: Edge -> Bound -> Bound
updateBound (a, b) (c, d) =
  let mab  = min a b
      mxab = max a b
  in  (min mab c, max mxab d)
