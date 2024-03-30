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

getDegrees :: EmbeddedMultigraph -> Int -> Degrees Int
getDegrees (EmbeddedMultigraph Undirected _ multiedges) n' =
    Degrees $ Data.Array.listArray (1, n') degrees
    where degrees = map (sum . neighbourMultiplicities) [1..n']
          adjustLoop i j = if i==j then (*2) else id
          neighbourMultiplicities i = [adjustLoop i j (multiplicity (createEdge Undirected i j) multiedges) | j <- [1..n']]

getDegrees (EmbeddedMultigraph Directed _ multiedges) n' =
    InOutDegrees (degrees inMult) (degrees outMult)
    where degrees mult = Data.Array.listArray (1, n') $ map (sum . mult) [1..n']
          outMult i = [multiplicity (createEdge Directed i j) multiedges | j <- [1..n']]
          inMult i = [multiplicity (createEdge Directed j i) multiedges | j <- [1..n']]

modularity :: EmbeddedMultigraph -> Degrees Double -> Double -> Layer -> Double
modularity (EmbeddedMultigraph Undirected _ multiedges) (Degrees degrees) m (lo, hi) =
    mr/m - (ds/(2*m))^(2::Integer)
    where mr = fromIntegral $ sum [multiplicity (createEdge Undirected i j) multiedges
                                    | i <- [lo..hi], j <- [lo..hi], i<=j]
          ds = (sum . map (degrees !)) [lo..hi]

modularity (EmbeddedMultigraph Directed _ multiedges) (InOutDegrees indegrees outdegrees) m (lo, hi) =
    mr/m - degSum indegrees * degSum outdegrees/(m^(2::Integer))
    where mr = fromIntegral $ sum [multiplicity (createEdge Directed i j) multiedges
                                     | i <- [lo..hi], j <- [lo..hi]]
          degSum ds = (sum . map (ds !)) [lo..hi]

modularity _ _ _ _ = error "Incompatible degrees and graph types."

pairModularity :: EmbeddedMultigraph -> Degrees Double -> Double -> (Int, Int) -> Double
pairModularity (EmbeddedMultigraph Undirected _ multiedges) (Degrees degrees) m (u, v) =
    let a_uv = fromIntegral $ multiplicity (createEdge Undirected u v) multiedges
        norm = if u/=v then id else (/ (2::Double))
    in (a_uv - norm (degrees!u) * (degrees!v) / m)/(2*m)

pairModularity (EmbeddedMultigraph Directed _ _) _ _ _ =
    error "pairModularity for directed graph is not yet supported."
-- pairModularity (EmbeddedMultigraph Directed _ multiedges) (InOutDegrees indegrees outdegrees) m (u, v) =
--     let a i j = fromIntegral $ multiplicity (Edge (i, j)) multiedges
--     in if u == v then
--         (a u v - (outdegrees!u) * (indegrees!v) / (2*m))/(2*m)
--     else
--         (a u v + a v u - ((outdegrees!u) * (indegrees!v) + (outdegrees!v) * (indegrees!u)) / (2*m))/(2*m)

pairModularity _ _ _ _ = error "Incompatible degrees and graph types."


type MemoizedFunction a b = (a -> HashMap a b -> (b, HashMap a b))
memoize :: (Hashable a, Show a) => (a -> b) -> MemoizedFunction a b
memoize f = g
    where g x mem =
            case Data.HashMap.Strict.lookup x mem of
                Just y -> (y, mem)
                Nothing -> let y = f x
                               mem' = Data.HashMap.Strict.insert x y mem
                           in (y, mem')

memoizePairSum :: (Num b, Show b) => MemoizedFunction Layer b -> ((Int, Int) -> b) -> Int -> MemoizedFunction Layer b
memoizePairSum f vertexF n' = g
    where g (k, j) m | k == j || j == n' = f (k, j) m
                     | otherwise = (0, m) `add` (k, j-1) `add` (k+1, j) `sub` (k+1, j-1) `addCombine` vertexF (k, j)
          add = memPairOp (+)
          sub = memPairOp (-)
          memPairOp op (y, mem) val =
                let (res, mem') = f val mem
                in (y`op`res, mem')
          addCombine (x, y) z = (x+z, y)
