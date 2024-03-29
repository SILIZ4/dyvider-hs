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

main :: IO ()
main = runProgram =<< execParser opts
  where
    opts = info (parser <**> helper)
      ( fullDesc
     <> progDesc "Exact linear community detection on embedded graphs.")

runProgram :: DyviderArgs -> IO ()
runProgram args = do
    (mapping, multigraph) <- sortMergeVertices <$> readGraphFile (filePath args)
    let n' = maximum mapping
        (EmbeddedMultigraph _ multiedges) = multigraph
        m' = sum $ map snd $ Data.HashMap.Strict.toList multiedges
        degrees = fromIntegral <$> getDegrees multigraph n'
        getMod = modularity multigraph degrees m'
        in print $ detectCommunities n' getMod
