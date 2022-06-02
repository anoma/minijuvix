module MiniJuvix.Syntax.MicroJuvix.ArityChecker.Error where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.MicroJuvix.ArityChecker.Error.Types

data ArityCheckerError
  = ArityCheckerError

-- instance ToGenericError ArityCheckerError where
--   genericError :: TypeCheckerError -> GenericError
--   genericError = \case
--     ErrTooManyPatterns e -> genericError e
--     ErrWrongConstructorType e -> genericError e
--     ErrWrongConstructorAppArgs e -> genericError e
--     ErrWrongType e -> genericError e
--     ErrUnsolvedMeta e -> genericError e
--     ErrExpectedFunctionType e -> genericError e
