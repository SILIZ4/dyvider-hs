module Main where

import Options.Applicative
import Data.Array (Array, (!))
import qualified Data.HashMap.Strict
import Data.List (foldl')
import Data.Maybe (fromJust)

import Dyvider
import Metrics
import Parse (readGraphFile)


data DyviderArgs = DyviderArgs
  { filePath    :: !FilePath
  , outputFile  :: !FilePath
  , quality     :: !String
  , directed    :: !Bool
  , memoizeF    :: !Bool
  , zeroIndexed :: !Bool
  }

parser :: Parser DyviderArgs
parser = DyviderArgs
      <$> argument str (
         metavar "FILE"
         <> help "Path to text file in csv-like format. The first line should contain the scores and the other lines should contain the edges.")
      <*> strOption
          ( long "output"
         <> short 'o'
         <> help "Output result to given file path."
         <> value ""
         <> metavar "OUTPUT_FILE" )
      <*> strOption
          ( long "quality"
         <> help "Quality metric used."
         <> showDefault
         <> value "modularity"
         <> metavar "{\"modularity\"}" )
      <*> switch
          ( long "directed"
         <> short 'd'
         <> help "Set if the graph is directed." )
      <*> switch
          ( long "memoize"
         <> short 'm'
         <> help "Edgelist is zero-indexed." )
      <*> switch
          ( long "zero"
         <> help "Edgelist is zero-indexed." )


layerID :: Array Int Int -> [Layer] -> [Int]
layerID mapping layers =
    [layerOf (mapping ! v) | v <- [1..n]]
    where n = length mapping
          layerOf x = fromJust $ foldl' (\acc (i, (ymin, ymax)) -> case acc of
                                            Just y -> Just y
                                            Nothing -> if x>=ymin && x<=ymax then Just i else Nothing)
                                 Nothing $ zip [1..] layers


runProgram :: DyviderArgs -> IO ()
runProgram args = do
    let orient = if directed args then Directed else Undirected
    let zero = zeroIndexed args
    (mapping, graph) <- sortMergeVertices <$> readGraphFile orient zero (filePath args)

    let n' = length mapping
        (EmbeddedGraph _ _ multiedges) = graph
        degrees = fromIntegral <$> getDegrees graph n'
        m' = fromIntegral $ edgeNumber multiedges
        f = case quality args of
                "modularity" -> modularity graph degrees m'
                _ -> error "Unsupported quality metric."
        fmem = case quality args of
                "modularity" -> memoizePairSum (memoize f) (pairModularity graph degrees m') n'
                _ -> error "Unsupported memoized quality metric."
        (qstar, partition) = if memoizeF args then
                        detectCommunitiesMem n' fmem Data.HashMap.Strict.empty
                    else
                        detectCommunities n' f
        filename = outputFile args
        fmem' x = fst (fmem x Data.HashMap.Strict.empty)
        in if filename == "" then
            putStrLn $ "Q*=" ++ show qstar ++ "\n" ++ show (layerID mapping partition)
        else
            writeFile filename $ show qstar ++ "\n" ++ show (layerID mapping partition)

main :: IO ()
main = runProgram =<< execParser opts
  where
    opts = info (parser <**> helper)
      ( fullDesc
     <> progDesc "Exact linear community detection on embedded graphs.")
