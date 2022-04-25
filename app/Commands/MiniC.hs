{-# LANGUAGE ApplicativeDo #-}

module Commands.MiniC where

import Commands.Extra
import MiniJuvix.Prelude hiding (Doc)
import Options.Applicative

newtype MiniCOptions = MiniCOptions
  { _mcInputFile :: FilePath
  }

parseMiniC :: Parser MiniCOptions
parseMiniC = do
  _mcInputFile <- parseInputFile
  pure MiniCOptions {..}
