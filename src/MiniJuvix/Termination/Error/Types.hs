module MiniJuvix.Termination.Error.Types where

import MiniJuvix.Prelude
import MiniJuvix.Prelude.Pretty
import MiniJuvix.Syntax.Abstract.Language
import MiniJuvix.Syntax.Concrete.Scoped.Name qualified as Scoped
import MiniJuvix.Termination.Error.Pretty

newtype FailedTerminationCheck = FailedTerminationCheck
  { _failedTerminationCheckFunName :: Name
  }
  deriving stock (Show)

makeLenses 'FailedTerminationCheck

instance ToGenericError FailedTerminationCheck where
  genericError FailedTerminationCheck {..} =
    Just
      GenericError
        { _genericErrorFile = i ^. intervalFile,
          _genericErrorLoc = intervalStartLoc i,
          _genericErrorMessage = prettyError msg,
          _genericErrorIntervals = [i]
        }
    where
      name = _failedTerminationCheckFunName
      i = getLoc name

      msg :: Doc Eann
      msg =
        "The function" <+> pretty (Scoped.nameUnqualifiedText name)
          <+> "fails the termination checker."
