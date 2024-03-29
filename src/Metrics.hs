module Metrics where

import qualified Data.Array
import Data.Array (Array, (!))

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
