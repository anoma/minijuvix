{-# LANGUAGE ApplicativeDo #-}

module Commands.MonoJuvix where

import Commands.Extra
import MiniJuvix.Prelude hiding (Doc)
import Options.Applicative

newtype MonoJuvixOptions = MonoJuvixOptions
  { _monojuvixInputFile :: FilePath
  }

parseMonoJuvix :: Parser MonoJuvixOptions
parseMonoJuvix = do
  _monojuvixInputFile <- parseInputFile
  pure MonoJuvixOptions {..}
