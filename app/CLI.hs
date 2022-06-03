module CLI
  ( module CLI,
    module GlobalOptions,
    module Command,
  )
where

import Command
import GlobalOptions
import MiniJuvix.Prelude hiding (Doc)
import Options.Applicative
import Options.Applicative.Help.Pretty

data CLI
  = -- Available options
    DisplayVersion
  | DisplayHelp
  | -- Available commands
    Command CommandGlobalOptions

parseDisplayVersion :: Parser CLI
parseDisplayVersion =
  flag'
    DisplayVersion
    (long "version" <> short 'v' <> help "Print the version and exit")

parseDisplayHelp :: Parser CLI
parseDisplayHelp =
  flag'
    DisplayHelp
    (long "help" <> short 'h' <> help "Show the help text")

data CommandGlobalOptions = CommandGlobalOptions
  { _cliCommand :: Command,
    _cliGlobalOptions :: GlobalOptions
  }

makeLenses ''CommandGlobalOptions

parseCommandGlobalOptions :: Parser CLI
parseCommandGlobalOptions = do
  _cliCommand <- parseCommand
  _globalNoColors <-
    switch
      ( long "no-colors"
          <> help "Disable globally ANSI formatting"
      )
  _globalShowNameIds <-
    switch
      ( long "show-name-ids"
          <> help "Show the unique number of each identifier when pretty printing"
      )
  _globalOnlyErrors <-
    switch
      ( long "only-errors"
          <> help "Only print errors in a uniform format (used by minijuvix-mode)"
      )
  _globalNoTermination <-
    switch
      ( long "no-termination"
          <> help "Disable the termination checker"
      )
  _globalInputFiles <- parseInputFiles
  pure (Command (CommandGlobalOptions {_cliGlobalOptions = GlobalOptions {..}, ..}))

parseCLI :: Parser CLI
parseCLI = parseDisplayVersion <|> parseDisplayHelp <|> parseCommandGlobalOptions

commandFirstFile :: CommandGlobalOptions -> Maybe FilePath
commandFirstFile CommandGlobalOptions {_cliGlobalOptions = GlobalOptions {..}} =
  listToMaybe _globalInputFiles

makeAbsPaths :: CLI -> IO CLI
makeAbsPaths cli = case cli of
  Command cmd -> do
    nOpts <- traverseOf globalInputFiles (mapM makeAbsolute) (cmd ^. cliGlobalOptions)
    return (Command (set cliGlobalOptions nOpts cmd))
  _ -> return cli

descr :: ParserInfo CLI
descr =
  info
    parseCLI
    ( fullDesc
        <> progDesc "The MiniJuvix compiler."
        <> headerDoc (Just headDoc)
        <> footerDoc (Just foot)
    )
  where
    headDoc :: Doc
    headDoc = dullblue $ bold $ underline "MiniJuvix help"

    foot :: Doc
    foot = bold "maintainers: " <> "The MiniJuvix Team"
