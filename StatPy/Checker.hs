{-
 -  HOPL/CHECKED/Checker.hs
 -
 -  Reference implementation of the toy language CHECKED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the static type checker implementation.
 -
 -  Author: Matthew A Johnson
 -}
module StatPy.Checker (check, checkWith) where

import HOPL.CHECKED.DataStructures (DenVal, Environment, ExpVal (..), Procedure (..))
import HOPL.CHECKED.Environment (Env (..))
import HOPL.CHECKED.Lang.Parser (ParseError, parseToplevel)
import HOPL.CHECKED.Lang.Syntax (Exp (..), Pgm (..))
import HOPL.CHECKED.TypeEnv
import HOPL.Types (Source)

check :: Source -> Either ParseError Pgm
check src = checkWith emptyTenv src

checkWith :: TypeEnvironment -> Source -> Either ParseError Pgm
checkWith ρ src = case result of
  Right prog -> typeOfProgram prog ρ `seq` result
  _ -> result
  where
    result = parseToplevel src

reportUnequalTypes :: Type -> Type -> Exp -> Type
reportUnequalTypes t₁ t₂ exp =
  error $
    "Types didn't match: "
      ++ show t₁
      ++ " /= "
      ++ show t₂
      ++ " in "
      ++ show (show exp)

typeOfProgram :: Pgm -> TypeEnvironment -> Type
typeOfProgram (Pgm e) ρ = typeOf e ρ

typeOf :: Exp -> TypeEnvironment -> Type
typeOf (ConstExp _) _ = IntType
typeOf (VarExp x) ρ = applyTenv ρ x
typeOf (IsZeroExp exp) ρ
  | t == IntType = BoolType
  | otherwise = reportUnequalTypes IntType t exp
  where
    t = typeOf exp ρ
typeOf (DiffExp exp₁ exp₂) ρ
  | t₁ /= IntType = reportUnequalTypes IntType t₁ exp₁
  | t₂ /= IntType = reportUnequalTypes IntType t₂ exp₂
  | otherwise = IntType
  where
    t₁ = typeOf exp₁ ρ
    t₂ = typeOf exp₂ ρ
typeOf (IfExp exp₁ exp₂ exp₃) ρ
  | t₁ /= BoolType = reportUnequalTypes BoolType t₁ exp₁
  | t₂ /= t₃ = reportUnequalTypes t₂ t₃ exp₂
  | otherwise = t₂
  where
    t₁ = typeOf exp₁ ρ
    t₂ = typeOf exp₂ ρ
    t₃ = typeOf exp₃ ρ
typeOf (LetExp var rhs body) ρ = typeOf body ρ'
  where
    ρ' = extendTenv var t ρ
    t = typeOf rhs ρ
typeOf (LetrecExp tres pname param targ pbody body) ρ
  | tres' == tres = typeOf body ρ'
  | otherwise = reportUnequalTypes tres tres' pbody
  where
    ρ' = extendTenv pname (ProcType targ tres) ρ
    tres' = typeOf pbody (extendTenv param targ ρ')
typeOf (ProcExp param targ body) ρ = ProcType targ tres
  where
    tres = typeOf body ρ'
    ρ' = extendTenv param targ ρ
typeOf (CallExp rator rand) ρ
  | targ == targ' = tres
  | otherwise = reportUnequalTypes targ targ' rand
  where
    ProcType targ tres = typeOf rator ρ
    targ' = typeOf rand ρ

{--- Auxiliary functions ---}
