module Data.ConnCompFixture where


import           Data.Set                                          as S
import           Relude                                            as R
import           Test.QuickCheck


newtype Edge a = Edge (a, a)
  deriving (Show, Eq, Ord)

newtype Graph = Graph { _gEdges :: Set (Edge Integer) } deriving (Show)

instance Arbitrary (Edge Integer) where
  arbitrary = do
    v1 <- getPositive <$> arbitrary
    v2 <- (getPositive <$> arbitrary) `suchThat` (/= v1)
    return $ Edge (v1, v2)

arbitraryGraphs :: Gen (Graph, Int)
arbitraryGraphs = do
  amount <- choose (1, 10)
  (, amount) <$> genGraph amount

genGraph :: Int -> Gen Graph
genGraph = fmap Graph . genConnComp

genConnComp :: Int -> Gen (Set (Edge Integer))
genConnComp amount = foldMap identity <$> foldlM combine [] [1 .. amount]
 where
  combine :: [Set (Edge Integer)] -> Int -> Gen [Set (Edge Integer)]
  combine set _ = do
    newSet <- genSetEdgesConnComp `suchThat` noIncidents set
    return $ newSet : set

noIncidents :: [Set (Edge Integer)] -> Set (Edge Integer) -> Bool
noIncidents origin target = getAll $ foldMap (All . flip noIncidents' target) origin

noIncidents' :: Set (Edge Integer) -> Set (Edge Integer) -> Bool
noIncidents' origin target = getAll $ foldMap (All . (\edge -> not (memberE edge origin))) target

memberE :: Edge Integer -> Set (Edge Integer) -> Bool
memberE e = getAny . foldMap (Any . isIncident e)

swapE :: Edge a -> Edge a
swapE (Edge (a, b)) = Edge (b, a)

isIncident :: Eq a => Edge a -> Edge a -> Bool
isIncident (Edge (a, b)) (Edge (c, d)) = a == c || b == c || a == d || b == d

genSetEdgesConnComp :: Gen (Set (Edge Integer))
genSetEdgesConnComp = do
  setSize <- choose (1, 20)
  fst <$> foldlM combine (S.empty, mempty) [1 .. setSize]

 where
  combine :: (S.Set (Edge Integer), Set Integer) -> Integer -> Gen (S.Set (Edge Integer), Set Integer)
  combine (set, cc) _ = do
    edge <- arbitrary `suchThat` (\e -> S.null cc || e `existsOn` cc)
    return (S.insert edge set, edge `addVertices` cc)

addVertices :: Ord a => Edge a -> Set a -> Set a
addVertices (Edge (a, b)) set = b `S.insert` (a `S.insert` set)

existsOn :: Ord a => Edge a -> Set a -> Bool
existsOn (Edge (a, b)) set = a `S.member` set || b `S.member` set

toEdgesText :: S.Set (Edge Integer) -> Text
toEdgesText = S.foldr (\(Edge (a, b)) bs -> bs <> show a <> " " <> show b <> " \n") ""
