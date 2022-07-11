module Command
  ( module Command,
    module Commands.Extra,
    module Commands.Html,
    module Commands.MicroJuvix,
    module Commands.Parse,
    module Commands.Scope,
    module Commands.Termination,
    module Commands.Compile,
  )
where

import Commands.Compile
import Commands.Extra
import Commands.Html
import Commands.MicroJuvix
import Commands.Parse
import Commands.Scope
import Commands.Termination
import GlobalOptions
import Juvix.Prelude hiding (Doc)
import Juvix.Syntax.Concrete.Scoped.Pretty qualified as Scoper
import Options.Applicative

data Command
  = -- Visible commands
    Check
  | Compile CompileOptions
  | Html HtmlOptions
  | -- Internal commands
    DisplayRoot
  | Highlight
  | MicroJuvix MicroJuvixCommand
  | MiniC
  | MiniHaskell
  | MonoJuvix
  | Parse ParseOptions
  | Scope ScopeOptions
  | Termination TerminationCommand

data CommandGlobalOptions = CommandGlobalOptions
  { _cliCommand :: Command,
    _cliGlobalOptions :: GlobalOptions
  }

makeLenses ''CommandGlobalOptions

parseCommandGlobalOptions :: Parser CommandGlobalOptions
parseCommandGlobalOptions = do
  opts <- parseGlobalFlags False
  cmd <-
    hsubparser
      ( mconcat
          [ commandCheck,
            commandCompile,
            commandHtml
          ]
      )
      <|> hsubparser
        ( internal
            <> mconcat
              [ commandHighlight,
                commandMicroJuvix,
                commandMiniC,
                commandMiniHaskell,
                commandMonoJuvix,
                commandParse,
                commandScope,
                commandShowRoot,
                commandTermination
              ]
        )
  return (cmd {_cliGlobalOptions = opts <> cmd ^. cliGlobalOptions})

commandCheck :: Mod CommandFields CommandGlobalOptions
commandCheck =
  command "check" $
    info
      (addGlobalOptions (pure Check))
      (progDesc "Type check a Juvix file")

commandCompile :: Mod CommandFields CommandGlobalOptions
commandCompile =
  command "compile" $
    info
      (addGlobalOptions (Compile <$> parseCompile))
      (progDesc "Compile a Juvix file")

commandHighlight :: Mod CommandFields CommandGlobalOptions
commandHighlight =
  command "highlight" $
    info
      (addGlobalOptions (pure Highlight))
      (progDesc "Highlight a Juvix file")

commandHtml :: Mod CommandFields CommandGlobalOptions
commandHtml =
  command "html" $
    info
      (addGlobalOptions (Html <$> parseHtml))
      (progDesc "Generate HTML for a Juvix file")

commandMiniC :: Mod CommandFields CommandGlobalOptions
commandMiniC =
  command "minic" $
    info
      (addGlobalOptions (pure MiniC))
      (progDesc "Translate a Juvix file to MiniC")

commandMicroJuvix :: Mod CommandFields CommandGlobalOptions
commandMicroJuvix =
  command "microjuvix" $
    info
      (addGlobalOptions (MicroJuvix <$> parseMicroJuvixCommand))
      (progDesc "Subcommands related to MicroJuvix")

commandMiniHaskell :: Mod CommandFields CommandGlobalOptions
commandMiniHaskell =
  command "minihaskell" $
    info
      (addGlobalOptions (pure MiniHaskell))
      (progDesc "Translate a Juvix file to MiniHaskell")

commandMonoJuvix :: Mod CommandFields CommandGlobalOptions
commandMonoJuvix =
  command "monojuvix" $
    info
      (addGlobalOptions (pure MonoJuvix))
      (progDesc "Translate a Juvix file to MonoJuvix")

commandParse :: Mod CommandFields CommandGlobalOptions
commandParse =
  command "parse" $
    info
      (addGlobalOptions (Parse <$> parseParse))
      (progDesc "Parse a Juvix file")

commandScope :: Mod CommandFields CommandGlobalOptions
commandScope =
  command "scope" $
    info
      (addGlobalOptions (Scope <$> parseScope))
      (progDesc "Parse and scope a Juvix file")

commandShowRoot :: Mod CommandFields CommandGlobalOptions
commandShowRoot =
  command "root" $
    info
      (liftParserCmd (pure DisplayRoot))
      (progDesc "Show the root path for a Juvix project")

commandTermination :: Mod CommandFields CommandGlobalOptions
commandTermination =
  command "termination" $
    info
      (addGlobalOptions $ Termination <$> parseTerminationCommand)
      (progDesc "Subcommands related to termination checking")

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

cmdDefaultOptions :: Command -> CommandGlobalOptions
cmdDefaultOptions _cliCommand =
  CommandGlobalOptions {_cliGlobalOptions = mempty, ..}

liftParserCmd :: Parser Command -> Parser CommandGlobalOptions
liftParserCmd cmd = cmdDefaultOptions <$> cmd

addGlobalOptions :: Parser Command -> Parser CommandGlobalOptions
addGlobalOptions parser = do
  flags1 <- parseGlobalFlags True
  ~(opts2, _cliCommand) <- addParser (parseGlobalOptions True) parser
  fs <- parserInputFiles
  return
    CommandGlobalOptions
      { _cliGlobalOptions = flags1 <> opts2 <> mempty {_globalInputFiles = fs},
        ..
      }

mkScopePrettyOptions :: GlobalOptions -> ScopeOptions -> Scoper.Options
mkScopePrettyOptions g ScopeOptions {..} =
  Scoper.defaultOptions
    { Scoper._optShowNameIds = g ^. globalShowNameIds,
      Scoper._optInlineImports = _scopeInlineImports
    }
