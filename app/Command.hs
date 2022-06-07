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
import MiniJuvix.Prelude hiding (Doc)
import MiniJuvix.Syntax.Concrete.Scoped.Pretty qualified as Scoper
import Options.Applicative

data Command
  = Compile CompileOptions
  | DisplayRoot
  | Highlight
  | Html HtmlOptions
  | MicroJuvix MicroJuvixCommand
  | MiniC
  | MiniHaskell
  | MonoJuvix
  | Parse ParseOptions
  | Scope ScopeOptions
  | Termination TerminationCommand

mkScopePrettyOptions :: GlobalOptions -> ScopeOptions -> Scoper.Options
mkScopePrettyOptions g ScopeOptions {..} =
  Scoper.defaultOptions
    { Scoper._optShowNameId = g ^. globalShowNameIds,
      Scoper._optInlineImports = _scopeInlineImports
    }

data CommandGlobalOptions = CommandGlobalOptions
  { _cliCommand :: Command,
    _cliGlobalOptions :: GlobalOptions
  }

makeLenses ''CommandGlobalOptions

parseCommand :: Parser CommandGlobalOptions
parseCommand =
  hsubparser
    ( mconcat
        [ commandCompile,
          commandHighlight,
          commandHtml,
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

noGlobals :: Parser Command -> Parser CommandGlobalOptions
noGlobals parseCmd = do
  _cliCommand <- parseCmd
  return CommandGlobalOptions {_cliGlobalOptions = mempty, ..}

addGlobals :: Parser Command -> Parser CommandGlobalOptions
addGlobals parseCmd = do
  _cliCommand <- parseCmd
  _cliGlobalOptions <- parseGlobalOptions
  return CommandGlobalOptions {..}

commandShowRoot :: Mod CommandFields CommandGlobalOptions
commandShowRoot =
  command "root" $
    info
      (noGlobals (pure DisplayRoot))
      (progDesc "Show the root path for a Minijuvix project")

commandMicroJuvix :: Mod CommandFields CommandGlobalOptions
commandMicroJuvix =
  command "microjuvix" $
    info
      (addGlobals (MicroJuvix <$> parseMicroJuvixCommand))
      (progDesc "Subcommands related to MicroJuvix")

commandMonoJuvix :: Mod CommandFields CommandGlobalOptions
commandMonoJuvix =
  command "monojuvix" $
    info
      (addGlobals (pure MonoJuvix))
      (progDesc "Translate a MiniJuvix file to MonoJuvix")

commandMiniHaskell :: Mod CommandFields CommandGlobalOptions
commandMiniHaskell =
  command "minihaskell" $
    info
      (addGlobals (pure MiniHaskell))
      (progDesc "Translate a MiniJuvix file to MiniHaskell")

commandMiniC :: Mod CommandFields CommandGlobalOptions
commandMiniC =
  command "minic" $
    info
      (addGlobals (pure MiniC))
      (progDesc "Translate a MiniJuvix file to MiniC")

commandCompile :: Mod CommandFields CommandGlobalOptions
commandCompile =
  command "compile" $
    info
      (addGlobals (Compile <$> parseCompile))
      (progDesc "Compile a MiniJuvix file")

commandHighlight :: Mod CommandFields CommandGlobalOptions
commandHighlight =
  command "highlight" $
    info
      (addGlobals (pure Highlight))
      (progDesc "Highlight a MiniJuvix file")

commandParse :: Mod CommandFields CommandGlobalOptions
commandParse =
  command "parse" $
    info
      (addGlobals (Parse <$> parseParse))
      (progDesc "Parse a MiniJuvix file")

commandHtml :: Mod CommandFields CommandGlobalOptions
commandHtml =
  command "html" $
    info
      (addGlobals (Html <$> parseHtml))
      (progDesc "Generate HTML for a MiniJuvix file")

commandScope :: Mod CommandFields CommandGlobalOptions
commandScope =
  command "scope" $
    info
      (addGlobals (Scope <$> parseScope))
      (progDesc "Parse and scope a MiniJuvix file")

commandTermination :: Mod CommandFields CommandGlobalOptions
commandTermination =
  command "termination" $
    info
      (addGlobals (Termination <$> parseTerminationCommand))
      (progDesc "Subcommands related to termination checking")
