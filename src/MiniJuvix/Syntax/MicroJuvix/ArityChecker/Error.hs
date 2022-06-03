module MiniJuvix.Syntax.MicroJuvix.ArityChecker.Error where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.Error.Pretty

-- import MiniJuvix.Syntax.MicroJuvix.ArityChecker.Error.Types

data ArityCheckerError
  = ArityCheckerError Interval
  | ErrFutureTypeCheckerError

instance ToGenericError ArityCheckerError where
  genericError :: ArityCheckerError -> GenericError
  genericError = \case
    ArityCheckerError i ->
      GenericError
        { _genericErrorLoc = i,
          _genericErrorMessage = prettyError "arity error",
          _genericErrorIntervals = [i]
        }
    ErrFutureTypeCheckerError {} -> error "future type check"
