module MiniJuvix.Syntax.Concrete.Scoped.InfoTable where

import GHC.Event (FdKey (keyFd))
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Concrete.Language
import MiniJuvix.Syntax.Concrete.Scoped.Name qualified as S

newtype FunctionInfo = FunctionInfo
  { _functionInfoType :: Expression
  }
  deriving stock (Eq, Show)

newtype ConstructorInfo = ConstructorInfo
  { _constructorInfoType :: Expression
  }
  deriving stock (Eq, Show)

data AxiomInfo = AxiomInfo
  { _axiomInfoType :: Expression
  }
  deriving stock (Eq, Show)

newtype InductiveInfo = InductiveInfo
  { _inductiveInfoDef :: InductiveDef 'Scoped
  }
  deriving stock (Eq, Show)

data InfoTable = InfoTable
  { _infoConstructors :: HashMap ConstructorRef ConstructorInfo,
    _infoAxioms :: HashMap AxiomRef AxiomInfo,
    _infoInductives :: HashMap InductiveRef InductiveInfo,
    _infoFunctions :: HashMap FunctionRef FunctionInfo,
    _infoFunctionClauses :: HashMap S.Symbol (FunctionClause 'Scoped),
    _infoNames :: [S.Name],
    _infoCompilationRules :: HashMap S.Name [BackendItem]
    -- TODO: consider S.Symbol as the type of the key
    --       also, better to have a dict instead of a list for the backends.
    --       maybe, create a data type for the values
  }
  deriving stock (Eq, Show)

emptyInfoTable :: InfoTable
emptyInfoTable =
  InfoTable
    { _infoConstructors = mempty,
      _infoAxioms = mempty,
      _infoInductives = mempty,
      _infoFunctions = mempty,
      _infoFunctionClauses = mempty,
      _infoNames = mempty,
      _infoCompilationRules = mempty
    }

makeLenses ''InfoTable
makeLenses ''InductiveInfo
makeLenses ''ConstructorInfo
makeLenses ''AxiomInfo
makeLenses ''FunctionInfo
