module MiniJuvix.Syntax.MicroJuvix.ArityChecker
  ( module MiniJuvix.Syntax.MicroJuvix.ArityChecker,
    module MiniJuvix.Syntax.MicroJuvix.MicroJuvixArityResult,
    module MiniJuvix.Syntax.MicroJuvix.ArityChecker.Error,
  )
where

import Data.HashMap.Strict qualified as HashMap
import MiniJuvix.Prelude hiding (fromEither)
import MiniJuvix.Syntax.MicroJuvix.ArityChecker.Error
import MiniJuvix.Syntax.MicroJuvix.InfoTable
import MiniJuvix.Syntax.MicroJuvix.Language.Extra
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixResult
import MiniJuvix.Syntax.MicroJuvix.MicroJuvixArityResult

entryMicroJuvixArity ::
  Member (Error ArityCheckerError) r =>
  MicroJuvixResult ->
  Sem r MicroJuvixArityResult
entryMicroJuvixArity res@MicroJuvixResult {..} = do
  r <- runReader table (mapM checkModule _resultModules)
  return
    MicroJuvixArityResult
      { _resultMicroJuvixResult = res,
        _resultModules = r
      }
  where
    table :: InfoTable
    table = buildTable _resultModules

checkModule ::
  Members '[Reader InfoTable, Error ArityCheckerError] r =>
  Module ->
  Sem r Module
checkModule Module {..} = do
  _moduleBody' <- checkModuleBody _moduleBody
  return
    Module
      { _moduleBody = _moduleBody',
        ..
      }

checkModuleBody ::
  Members '[Reader InfoTable, Error ArityCheckerError] r =>
  ModuleBody ->
  Sem r ModuleBody
checkModuleBody ModuleBody {..} = do
  _moduleStatements' <- mapM checkStatement _moduleStatements
  return
    ModuleBody
      { _moduleStatements = _moduleStatements'
      }

checkInclude ::
  Members '[Reader InfoTable, Error ArityCheckerError] r =>
  Include ->
  Sem r Include
checkInclude = traverseOf includeModule checkModule

checkStatement ::
  Members '[Reader InfoTable, Error ArityCheckerError] r =>
  Statement ->
  Sem r Statement
checkStatement s = case s of
  StatementFunction fun -> StatementFunction <$> checkFunctionDef fun
  StatementForeign {} -> return s
  StatementInductive {} -> return s
  StatementInclude i -> StatementInclude <$> checkInclude i
  StatementAxiom {} -> return s

checkFunctionDef ::
  Members '[Reader InfoTable, Error ArityCheckerError] r =>
  FunctionDef ->
  Sem r FunctionDef
checkFunctionDef FunctionDef {..} = do
  info <- lookupFunction _funDefName
  _funDefClauses' <- mapM (checkFunctionClause info) _funDefClauses
  return
    FunctionDef
      { _funDefClauses = _funDefClauses',
        ..
      }

checkFunctionClause :: Members '[Reader InfoTable, Error ArityCheckerError] r =>
  FunctionInfo ->
  FunctionClause ->
  Sem r FunctionClause
checkFunctionClause = undefined
