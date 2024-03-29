module Main where

import Options.Applicative
import Data.Array (Array, (!))

import Dyvider
import Parse (readGraphFile)


data DyviderArgs = DyviderArgs
  { filePath    :: !FilePath
  , directed    :: !Bool
  , zeroIndexed :: !Bool
  , quality     :: !String }

parser :: Parser DyviderArgs
parser = DyviderArgs
      <$> argument str (
         metavar "FILE"
         <> help "Path to text file in csv-like format. The first line should contain the scores and the other lines should contain the edges.")
      <*> switch
          ( long "directed"
         <> short 'd'
         <> help "Set if the graph is directed." )
      <*> switch
          ( long "zero"
         <> help "Edgelist is zero-indexed." )
      <*> option auto
          ( long "quality"
         <> help "Quality metric used."
         <> showDefault
         <> value "modularity"
         <> metavar "{\"modularity\"}" )


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
        getMod = modularity multigraph degrees (edgeNumber multiedges)
        (qstar, partition) = detectCommunities n' getMod
        in putStrLn $ "Q*=" ++ show qstar ++ "\n"
            ++ show [map (originalLabels zero mapping) [lo..hi]| (lo, hi) <- partition]

main :: IO ()
main = runProgram =<< execParser opts
  where
    opts = info (parser <**> helper)
      ( fullDesc
     <> progDesc "Exact linear community detection on embedded graphs.")
