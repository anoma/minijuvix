module MiniJuvix.Syntax.MicroJuvix.ArityChecker.Error.Types where

-- import MiniJuvix.Syntax.MicroJuvix.Error.Pretty
import MiniJuvix.Prelude
import MiniJuvix.Prelude.Pretty
import MiniJuvix.Syntax.MicroJuvix.Error.Pretty
import MiniJuvix.Syntax.MicroJuvix.Language

data WrongConstructorAppLength = WrongConstructorAppArgs
  { _wrongConstructorAppLength :: ConstructorApp,
    _wrongConstructorAppLengthExpected :: Int
  }

makeLenses ''WrongConstructorAppLength

instance ToGenericError WrongConstructorAppLength where
  genericError e =
    GenericError
      { _genericErrorLoc = i,
        _genericErrorMessage = prettyError msg,
        _genericErrorIntervals = [i]
      }
    where
      i = getLoc (e ^. wrongConstructorAppLength)
      msg =
        "In the pattern " <+> ppCode (e ^. wrongConstructorAppLength) <+> "the constructor"
          <+> ppCode (e ^. wrongConstructorAppLength . constrAppConstructor)
          <+> "expected"
          <+> pat (e ^. wrongConstructorAppLengthExpected)

      pat :: Int -> Doc ann
      pat n = pretty n <+> plural "pattern" "patterns" n
