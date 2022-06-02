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

parseCommand :: Parser Command
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

commandShowRoot :: Mod CommandFields Command
commandShowRoot =
  command "root" $
    info
      (pure DisplayRoot)
      (progDesc "Show the root path of a MiniJuvix file")

commandMicroJuvix :: Mod CommandFields Command
commandMicroJuvix =
  command "microjuvix" $
    info
      (MicroJuvix <$> parseMicroJuvixCommand)
      (progDesc "Subcommands related to MicroJuvix")

commandMonoJuvix :: Mod CommandFields Command
commandMonoJuvix =
  command "monojuvix" $
    info
      (pure MonoJuvix)
      (progDesc "Translate a MiniJuvix file to MonoJuvix")

commandMiniHaskell :: Mod CommandFields Command
commandMiniHaskell =
  command "minihaskell" $
    info
      (pure MiniHaskell)
      (progDesc "Translate a MiniJuvix file to MiniHaskell")

commandMiniC :: Mod CommandFields Command
commandMiniC =
  command "minic" $
    info
      (pure MiniC)
      (progDesc "Translate a MiniJuvix file to MiniC")

commandCompile :: Mod CommandFields Command
commandCompile =
  command "compile" $
    info
      (Compile <$> parseCompile)
      (progDesc "Compile a MiniJuvix file")

commandHighlight :: Mod CommandFields Command
commandHighlight =
  command "highlight" $
    info
      (pure Highlight)
      (progDesc "Highlight a MiniJuvix file")

commandParse :: Mod CommandFields Command
commandParse =
  command "parse" $
    info
      (Parse <$> parseParse)
      (progDesc "Parse a MiniJuvix file")

commandHtml :: Mod CommandFields Command
commandHtml =
  command "html" $
    info
      (Html <$> parseHtml)
      (progDesc "Generate HTML for a MiniJuvix file")

commandScope :: Mod CommandFields Command
commandScope =
  command "scope" $
    info
      (Scope <$> parseScope)
      (progDesc "Parse and scope a MiniJuvix file")

commandTermination :: Mod CommandFields Command
commandTermination =
  command "termination" $
    info
      (Termination <$> parseTerminationCommand)
      (progDesc "Subcommands related to termination checking")
