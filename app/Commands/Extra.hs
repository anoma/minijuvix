module Commands.Extra where

import MiniJuvix.Prelude hiding (Doc)
import Options.Applicative

parserInputFile :: Parser FilePath
parserInputFile =
  argument
    str
    ( metavar "MINIJUVIX_FILE"
        <> help "Path to a .mjuvix file"
        <> action "file"
    )

parseInputFiles :: Parser (NonEmpty FilePath)
parseInputFiles = some1 parserInputFile
