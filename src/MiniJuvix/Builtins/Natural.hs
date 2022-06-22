module MiniJuvix.Builtins.Natural where

import Data.HashSet qualified as HashSet
import MiniJuvix.Builtins
import MiniJuvix.Internal.NameIdGen
import MiniJuvix.Prelude
import MiniJuvix.Syntax.Abstract.Language.Extra
import MiniJuvix.Syntax.Abstract.Pretty

registerNaturalDef :: Member Builtins r => InductiveDef -> Sem r ()
registerNaturalDef d = do
  unless (null (d ^. inductiveParameters)) (error "Naturals should have no type parameters")
  unless (d ^. inductiveType === smallUniverse) (error "Naturals should be in the small universe")
  registerBuiltin BuiltinsNatural (d ^. inductiveName)
  case d ^. inductiveConstructors of
    [c1, c2] -> registerZero c1 >> registerSuc c2
    _ -> error "Natural numbers should have exactly two constructors"

registerZero :: Member Builtins r => InductiveConstructorDef -> Sem r ()
registerZero (InductiveConstructorDef zero ty) = do
  nat <- getBuiltin BuiltinsNatural
  unless (ty === nat) (error $ "zero has the wrong type " <> ppSimple ty <> " | " <> ppSimple nat)
  registerBuiltin BuiltinsZero zero

registerSuc :: Member Builtins r => InductiveConstructorDef -> Sem r ()
registerSuc (InductiveConstructorDef suc ty) = do
  nat <- getBuiltin BuiltinsNatural
  unless (ty === (nat --> nat)) (error "suc has the wrong type")
  registerBuiltin BuiltinsSuc suc

registerNaturalPlus :: Members '[Builtins, NameIdGen] r => FunctionDef -> Sem r ()
registerNaturalPlus f = do
  nat <- getBuiltin BuiltinsNatural
  zero <- toExpression <$> getBuiltin BuiltinsZero
  suc <- toExpression <$> getBuiltin BuiltinsSuc
  let plus = f ^. funDefName
      ty = f ^. funDefTypeSig
  unless (ty === (nat --> nat --> nat)) (error "Natural plus has the wrong type signature")
  registerBuiltin BuiltinsNaturalPlus plus
  n <- freshVar "n"
  m <- freshVar "m"
  let freeVars = HashSet.fromList [n, m]
      a =% b = (a ==% b) freeVars
      (.+.) :: (IsExpression a, IsExpression b) => a -> b -> Expression
      x .+. y = plus @@ x @@ y
      exClauses :: [(Expression, Expression)]
      exClauses =
        [ (zero .+. m, toExpression m),
          ((suc @@ n) .+. m, suc @@ (n .+. m))
        ]
      clauses :: [(Expression, Expression)]
      clauses =
        [ (clauseLhsAsExpression c, c ^. clauseBody)
          | c <- toList (f ^. funDefClauses)
        ]
  case zipExactMay exClauses clauses of
    Nothing -> error "Natural plus has the wrong number of clauses"
    Just z -> forM_ z $ \((exLhs, exBody), (lhs, body)) -> do
      unless (exLhs =% lhs) (error "clause lhs does not match")
      unless (exBody =% body) (error $ "clause body does not match " <> ppSimple exBody <> " | " <> ppSimple body)
