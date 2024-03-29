module Parse where

import Data.List (foldl')

import qualified Data.HashSet
import qualified Data.Array

import Dyvider
import Text.Read (readMaybe)


splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen cond = foldr (\x yss -> if cond x then []:yss else (x:head yss): tail yss) [[]]

readCsvLine :: Read a => [Char] -> [Maybe a]
readCsvLine = map (readMaybe . filter (/=' ')). filter (not . null) . splitWhen (==',')

maybeError :: String -> Maybe a -> a
maybeError _ (Just x) = x
maybeError msg Nothing = error msg

parseEdgeList :: Orientation -> Bool -> [String] -> SimpleAdjacencyMatrix
parseEdgeList orient zero edgesStr =
    adjacency
    where edges = map (map (maybeError "Couldn't read vertex index in edge list.") . readCsvLine) edgesStr
          symEdges = let convert [y, y'] = createEdge orient y y'
                         convert _ = error "A line in the edge list doesn't have two values."
                     in map (adjustVertices . convert) edges
          adjustVertices = if zero then (\(Edge (x, y)) -> Edge (x+1, y+1)) else id
          adjacency = foldl' (flip Data.HashSet.insert) Data.HashSet.empty symEdges

readGraphFile :: Orientation -> Bool -> String -> IO EmbeddedGraph
readGraphFile orient zero fileName = parseGraph . lines <$> readFile fileName
    where parseGraph xs =
            let (scoreLine, edgeLines) = case xs of
                            [_] -> error "File contains only the score line."
                            [] -> error "File empty."
                            (y:ys) -> (y, ys)
                scores = map (maybeError "Couldn't read score.") $ readCsvLine scoreLine
                n = length scores
                edges = parseEdgeList orient zero edgeLines
            in EmbeddedGraph orient (Data.Array.listArray (1, n) scores) edges
