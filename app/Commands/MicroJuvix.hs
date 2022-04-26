{-# LANGUAGE ApplicativeDo #-}

module Commands.MicroJuvix where

import Commands.Extra
import MiniJuvix.Prelude hiding (Doc)
import Options.Applicative

data MicroJuvixCommand
  = Pretty MicroJuvixPrettyOptions
  | TypeCheck MicroJuvixTypeOptions

newtype MicroJuvixPrettyOptions = MicroJuvixPrettyOptions
  { _mjuvixPrettyInputFile :: FilePath
  }

data MicroJuvixTypeOptions = MicroJuvixTypeOptions
  { _mjuvixTypeInputFile :: FilePath,
    _mjuvixTypePrint :: Bool
  }

makeLenses ''MicroJuvixPrettyOptions
makeLenses ''MicroJuvixTypeOptions

parseMicroJuvixCommand :: Parser MicroJuvixCommand
parseMicroJuvixCommand =
  hsubparser $
    mconcat
      [ commandPretty,
        commandTypeCheck
      ]
  where
    commandPretty :: Mod CommandFields MicroJuvixCommand
    commandPretty = command "pretty" prettyInfo

    commandTypeCheck :: Mod CommandFields MicroJuvixCommand
    commandTypeCheck = command "typecheck" typeCheckInfo

    prettyInfo :: ParserInfo MicroJuvixCommand
    prettyInfo =
      info
        (Pretty <$> parseMicroJuvixPretty)
        (progDesc "Translate a MiniJuvix file to MicroJuvix and pretty print the result")

    typeCheckInfo :: ParserInfo MicroJuvixCommand
    typeCheckInfo =
      info
        (TypeCheck <$> parseMicroJuvixType)
        (progDesc "Translate a MiniJuvix file to MicroJuvix and typecheck the result")

parseMicroJuvixPretty :: Parser MicroJuvixPrettyOptions
parseMicroJuvixPretty = do
  _mjuvixPrettyInputFile <- parseInputFile
  pure MicroJuvixPrettyOptions {..}

parseMicroJuvixType :: Parser MicroJuvixTypeOptions
parseMicroJuvixType = do
  _mjuvixTypeInputFile <- parseInputFile
  _mjuvixTypePrint <-
    switch
      ( long "print-result"
          <> help "Print the type checked module if successful"
      )
  pure MicroJuvixTypeOptions {..}
