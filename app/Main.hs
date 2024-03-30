module Main where

import Options.Applicative
import Data.Array (Array, (!))
import qualified Data.HashMap.Strict

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


originalLabels :: Bool -> Array Int Int -> Int -> Int
originalLabels True mapping i = (mapping ! i) - 1
originalLabels False mapping i = mapping ! i

runProgram :: DyviderArgs -> IO ()
runProgram args = do
    let orient = if directed args then Directed else Undirected
    let zero = zeroIndexed args
    (mapping, multigraph) <- sortMergeVertices <$> readGraphFile orient zero (filePath args)

    let n' = maximum mapping
        (EmbeddedMultigraph _ _ multiedges) = multigraph
        degrees = fromIntegral <$> getDegrees multigraph n'
        m' = fromIntegral $ edgeNumber multiedges
        f = case quality args of
                "modularity" -> modularity multigraph degrees m'
                _ -> error "Unsupported quality metric."
        fmem = case quality args of
                "modularity" -> memoizePairSum (memoize f) (pairModularity multigraph degrees m') n'
                _ -> error "Unsupported memoized quality metric."
        (qstar, partition) = if memoizeF args then
                        detectCommunitiesMem n' fmem Data.HashMap.Strict.empty
                    else
                        detectCommunities n' f
        remappedPartition = [map (originalLabels zero mapping) [lo..hi]| (lo, hi) <- partition]
        filename = outputFile args
        in if filename == "" then
            putStrLn $ "Q*=" ++ show qstar ++ "\n" ++ show remappedPartition
        else
            writeFile filename (show qstar ++ "\n" ++ show remappedPartition)

main :: IO ()
main = runProgram =<< execParser opts
  where
    opts = info (parser <**> helper)
      ( fullDesc
     <> progDesc "Exact linear community detection on embedded graphs.")
