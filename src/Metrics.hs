module Metrics where

import qualified Data.Array
import Data.Array (Array, (!))
import qualified Data.HashMap.Strict
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)

import Dyvider


data Degrees a = Degrees !(Array Int a) | InOutDegrees !(Array Int a) !(Array Int a) deriving (Eq, Show)
instance Functor Degrees where
    fmap f (Degrees x) = Degrees (fmap f x)
    fmap f (InOutDegrees x y) = InOutDegrees (fmap f x) (fmap f y)

getDegrees :: EmbeddedGraph -> Int -> Degrees Int
getDegrees (EmbeddedGraph Undirected _ multiedges) n' =
    Degrees $ Data.Array.listArray (1, n') degrees
    where degrees = map (sum . neighbourMultiplicities) [1..n']
          adjustLoop i j = if i==j then (*2) else id
          neighbourMultiplicities i = [adjustLoop i j (multiplicity (createEdge Undirected i j) multiedges) | j <- [1..n']]

getDegrees (EmbeddedGraph Directed _ multiedges) n' =
    InOutDegrees (degrees inMult) (degrees outMult)
    where degrees mult = Data.Array.listArray (1, n') $ map (sum . mult) [1..n']
          outMult i = [multiplicity (Edge (i, j)) multiedges | j <- [1..n']]
          inMult i = [multiplicity (Edge (j, i)) multiedges | j <- [1..n']]

modularity :: EmbeddedGraph -> Degrees Double -> Double -> Layer -> Double
modularity (EmbeddedGraph Undirected _ multiedges) (Degrees degrees) m (lo, hi) =
    mr/m - (ds/(2*m))^(2::Integer)
    where mr = fromIntegral $ sum [multiplicity (createEdge Undirected i j) multiedges
                                    | i <- [lo..hi], j <- [lo..hi], i<=j]
          ds = (sum . map (degrees !)) [lo..hi]

modularity (EmbeddedGraph Directed _ multiedges) (InOutDegrees indegrees outdegrees) m (lo, hi) =
    mr/m - degSum indegrees * degSum outdegrees/(m^(2::Integer))
    where mr = fromIntegral $ sum [multiplicity (Edge (i, j)) multiedges
                                     | i <- [lo..hi], j <- [lo..hi]]
          degSum ds = (sum . map (ds !)) [lo..hi]

modularity _ _ _ _ = error "Incompatible degrees and graph types."

pairModularity :: EmbeddedGraph -> Degrees Double -> Double -> (Int, Int) -> Double
pairModularity (EmbeddedGraph Undirected _ multiedges) (Degrees degrees) m (u, v) =
    let a_uv = fromIntegral $ multiplicity (createEdge Undirected u v) multiedges
    in (a_uv - (degrees!u) * (degrees!v) / (2*m))/m

pairModularity (EmbeddedGraph Directed _ multiedges) (InOutDegrees indegrees outdegrees) m (u, v) =
    let a i j = fromIntegral $ multiplicity (Edge (i, j)) multiedges in
    if u == v then
        (a u v - (outdegrees!u) * (indegrees!v) / m)/m
    else
        (a u v + a v u - ((outdegrees!u) * (indegrees!v) + (outdegrees!v) * (indegrees!u)) / m)/m

pairModularity _ _ _ _ = error "Incompatible degrees and graph types."


type MemoizedFunction a b = (a -> HashMap a b -> (b, HashMap a b))
memoize :: (Hashable a) => (a -> b) -> MemoizedFunction a b
memoize f = g
    where g x mem =
            case Data.HashMap.Strict.lookup x mem of
                Just y -> (y, mem)
                Nothing -> let y = f x
                               mem' = Data.HashMap.Strict.insert x y mem
                           in (y, mem')

memoizePairSum :: (Num b) => MemoizedFunction Layer b -> ((Int, Int) -> b) -> Int -> MemoizedFunction Layer b
memoizePairSum f vertexF n' = g
    where g (k, j) m | k == j || j == n' = f (k, j) m
                     | otherwise = (0, m) `addf` (k, j-1) `addf` (k+1, j) `subf` (k+1, j-1) `add` vertexF (k, j)
          addf = memPairOp (+)
          subf = memPairOp (-)
          memPairOp op (y, mem) val =
                let (res, mem') = f val mem
                in (y`op`res, mem')
          add (x, y) z = (x+z, y)
