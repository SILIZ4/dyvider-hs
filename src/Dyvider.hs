module Dyvider where

import Data.Maybe (fromMaybe)
import Data.Foldable (toList, maximumBy)
import Data.List (foldl')

import qualified Data.Array
import Data.Array (Array, (!))
import qualified Data.HashMap.Strict
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import qualified Data.Map
import qualified Data.Hashable
import qualified Data.HashSet


data Orientation = Undirected | Directed deriving (Ord, Eq, Show)

newtype Edge = Edge (Int, Int) deriving (Ord, Eq, Show)
instance Data.Hashable.Hashable Edge where
    hashWithSalt x (Edge y) = Data.Hashable.hashWithSalt x y
    hash (Edge y) = Data.Hashable.hash y

type SimpleAdjacencyMatrix = HashSet Edge
data EmbeddedGraph = EmbeddedGraph !Orientation !(Array Int Double) !SimpleAdjacencyMatrix

type AdjacencyMatrix = HashMap Edge Int
data EmbeddedMultigraph = EmbeddedMultigraph !Orientation !(Array Int Double) !AdjacencyMatrix

multiplicity :: Edge -> AdjacencyMatrix -> Int
multiplicity e es = fromMaybe 0 $ Data.HashMap.Strict.lookup e es

createEdge :: Orientation -> Int -> Int -> Edge
createEdge Undirected x y | x <= y = Edge (y, x)
                       | otherwise = Edge (x, y)
createEdge Directed x y = Edge (x, y)


sortMergeVertices :: EmbeddedGraph -> (Array Int Int, EmbeddedMultigraph)
sortMergeVertices (EmbeddedGraph orient scores edges) =
    (mapping, EmbeddedMultigraph orient scores' multiedges)
    where scoreMap = Data.Map.fromList $ zip (toList scores) [1..]
          n' = Data.Map.size scoreMap
          scores' = Data.Array.listArray (1, n') . map fst $ Data.Map.toList scoreMap
          getIndex v = case Data.Map.lookup v scoreMap of
                        Nothing -> error "Score map was not created sucessfully."
                        Just x -> x
          mapping = fmap getIndex scores'
          multiedges = let
              newEdge (Edge (x, y)) = createEdge orient (mapping ! x) (mapping ! y)
              incrementEdge es e = Data.HashMap.Strict.insert (newEdge e) (multiplicity e es + 1) es
              in foldl' incrementEdge Data.HashMap.Strict.empty $ Data.HashSet.toList edges

type Layer = (Int, Int)

getDegrees :: EmbeddedMultigraph -> Int -> Array Int Int
getDegrees (EmbeddedMultigraph orient _ multiedges) n' =
    Data.Array.listArray (1, n') $ map (sum . neighbourMultiplicities) [1..n']
    where adjustLoop i j = if orient == Directed || i/=j then id else (*2)
          neighbourMultiplicities i = [adjustLoop i j (multiplicity (createEdge orient i j) multiedges) | j <- [1..n']]

modularity :: EmbeddedMultigraph -> Array Int Double -> Int -> Layer -> Double
modularity (EmbeddedMultigraph orient _ multiedges) degrees edgeNumber (lo, hi) =
    mr/m - (ds/(2*m))^(2::Integer)
    where mr = fromIntegral $ sum [multiplicity (createEdge orient i j) multiedges
                                    | i <- [lo..hi], j <- [lo..hi], i<=j]
          ds = (sum . map (degrees !)) [lo..hi]
          m = fromIntegral edgeNumber

headOr :: a -> [a] -> a
headOr x [] = x
headOr _ (x:_) = x

detectCommunities :: (Num a, Ord a, Show a) => Int -> (Layer -> a) -> (a, [Layer])
detectCommunities n' f = ((snd . head) bests, buildSolution bests n')
    where qStar j = maximumBy (\x y -> compare (snd x) (snd y)) . zipWith (\k q -> (k, q + f (k, j))) [j, j-1 .. 1]
          bests = foldl' (\qs j -> qStar j (map snd qs) : qs) [(1, 0)] [1 .. n']
          buildSolution [] _ = []
          buildSolution [_] _ = []
          buildSolution ((k, _):xs) kprev =
            (k, kprev): buildSolution (drop (kprev-k) xs) (k-1)
