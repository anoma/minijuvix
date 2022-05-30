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

data CLI = CLI
  { _cliGlobalOptions :: GlobalOptions,
    _cliCommand :: Command
  }

makeLenses ''CLI

parseCLI :: Parser CLI
parseCLI = do
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
  _cliCommand <- parseCommand
  _globalInputFiles <- parseInputFiles
  pure CLI {_cliGlobalOptions = GlobalOptions {..}, ..}

cliMainFile :: CLI -> FilePath
cliMainFile CLI {_cliGlobalOptions = GlobalOptions {..}} = head _globalInputFiles

makeAbsPaths :: CLI -> IO CLI
makeAbsPaths cli = do
  nOpts <- traverseOf globalInputFiles (mapM makeAbsolute) (cli ^. cliGlobalOptions)
  return (set cliGlobalOptions nOpts cli)

descr :: ParserInfo CLI
descr =
  info
    (parseCLI <**> helper)
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
