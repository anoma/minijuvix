module MiniJuvix.Builtins (
module MiniJuvix.Builtins,
) where

import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.Language.Extra

data ConstructorDescr = ConstructorDescr {
  constructorDescrName :: Name,
  constructorDescrType :: Expression
  }

data BuiltinsEnum =
  BuiltinsNatural
  | BuiltinsZero
  | BuiltinsSuc
  | BuiltinsNaturalPlus
  deriving stock (Eq, Generic)

instance Hashable BuiltinsEnum

data Builtins m a where
  GetBuiltin :: BuiltinsEnum -> Builtins m Name
  RegisterBuiltin :: BuiltinsEnum -> Name -> Builtins m ()

makeSem ''Builtins

newtype BuiltinsState = BuiltinsState {
  _builtinsTable :: HashMap BuiltinsEnum Name
  }
makeLenses ''BuiltinsState

iniState :: BuiltinsState
iniState = BuiltinsState mempty

re :: Sem (Builtins ': r) a -> Sem (State BuiltinsState ': r) a
re = reinterpret $ \case
  GetBuiltin n -> fromMaybe notReg <$> gets (^. builtinsTable . at n)
  RegisterBuiltin b n -> do
    s <- gets (^. builtinsTable . at b)
    case s of
      Nothing -> modify (over builtinsTable (set (at b) (Just n)))
      Just {} -> alreadyReg
  where
  notReg :: a
  notReg = error "not registered"
  alreadyReg :: a
  alreadyReg = error "already registered"

runBuiltins :: Sem (Builtins ': r) a -> Sem r a
runBuiltins = evalState iniState . re
