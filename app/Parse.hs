module Parse where

import Data.List (foldl')

import qualified Data.HashSet
import qualified Data.Array

import Dyvider


splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen cond = foldr (\x yss -> if cond x then []:yss else (x:head yss): tail yss) [[]]

readCsvLine :: Read a => [Char] -> [a]
readCsvLine = map (read . filter (/=' ')). filter (not . null) . splitWhen (==',')

graphSize :: [[Int]] -> Int
graphSize = maximum . map maximum

parseEdgeList :: [String] -> (Int, SimpleAdjacencyMatrix)
parseEdgeList edgesStr =
    (n, adjacency)
    where edges = map readCsvLine edgesStr
          n = graphSize edges
          symEdges = let convert [y, y'] = sym y y'
                         convert _ = error "Line contains more than two values."
                     in map convert edges
          adjacency = foldl' (flip Data.HashSet.insert) Data.HashSet.empty symEdges

readGraphFile :: String -> IO EmbeddedGraph
readGraphFile fileName = parseGraph . lines <$> readFile fileName
    where parseGraph xs =
            let (n, graph) = parseEdgeList $ tail xs
            in EmbeddedGraph (Data.Array.listArray (1, n) (readCsvLine (head xs))) graph
