module MiniJuvix.Syntax.MicroJuvix.Error.Pretty
  ( module MiniJuvix.Syntax.MicroJuvix.Error.Pretty,
    module MiniJuvix.Syntax.MicroJuvix.Error.Pretty.Ann,
  )
where

import MiniJuvix.Prelude
import MiniJuvix.Prelude.Pretty
import MiniJuvix.Syntax.MicroJuvix.Error.Pretty.Ann
import MiniJuvix.Syntax.MicroJuvix.Error.Pretty.Ansi qualified as Ansi
import MiniJuvix.Syntax.MicroJuvix.Pretty.Base qualified as Micro

ppCode :: Micro.PrettyCode c => c -> Doc Eann
ppCode = reAnnotate MicroAnn . Micro.runPrettyCode Micro.defaultOptions

newtype PPOutput = PPOutput (Doc Eann)

prettyError :: Doc Eann -> AnsiText
prettyError = AnsiText . PPOutput

instance HasAnsiBackend PPOutput where
  toAnsiStream (PPOutput o) = reAnnotateS Ansi.stylize (layoutPretty defaultLayoutOptions o)
  toAnsiDoc (PPOutput o) = reAnnotate Ansi.stylize o

instance HasTextBackend PPOutput where
  toTextDoc (PPOutput o) = unAnnotate o
  toTextStream (PPOutput o) = unAnnotateS (layoutPretty defaultLayoutOptions o)

indent' :: Doc ann -> Doc ann
indent' = indent 2

highlight :: Doc Eann -> Doc Eann
highlight = annotate Highlight
