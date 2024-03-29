module Main where

import qualified Data.HashMap.Strict
import Options.Applicative

import Dyvider
import Parse (readGraphFile)


data DyviderArgs = DyviderArgs
  { filePath :: !FilePath
  , directed :: !Bool
  , quality  :: !String }

parser :: Parser DyviderArgs
parser = DyviderArgs
      <$> argument str (
         metavar "FILE"
         <> help "Path to text file in csv-like format. The first line should contain the scores and the other lines should contain the edges.")
      <*> switch
          ( long "directed"
         <> short 'd'
         <> help "Set if the graph is directed." )
      <*> option auto
          ( long "quality"
         <> help "Quality metric used."
         <> showDefault
         <> value "modularity"
         <> metavar "{\"modularity\"}" )


runProgram :: DyviderArgs -> IO ()
runProgram args = do
    let orient = if directed args then Directed else Undirected
    (mapping, multigraph) <- sortMergeVertices <$> readGraphFile orient (filePath args)

    let n' = maximum mapping
        (EmbeddedMultigraph _ _ multiedges) = multigraph
        m' = sum $ map snd $ Data.HashMap.Strict.toList multiedges
        degrees = fromIntegral <$> getDegrees multigraph n'
        getMod = modularity multigraph degrees m'
        in print $ detectCommunities n' getMod

main :: IO ()
main = runProgram =<< execParser opts
  where
    opts = info (parser <**> helper)
      ( fullDesc
     <> progDesc "Exact linear community detection on embedded graphs.")
