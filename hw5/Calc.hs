{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import qualified ExprT as E
import Parser
import qualified StackVM as S
import qualified Data.Map as M

eval :: E.ExprT -> Integer
eval (E.Lit n)   = n
eval (E.Add a b) = (eval a) + (eval b)
eval (E.Mul a b) = (eval a) * (eval b)

evalStr :: String -> Maybe Integer
evalStr = helper . parseExp E.Lit E.Add E.Mul
    where
        helper (Just f) = Just (eval f)
        helper Nothing  = Nothing

class Expr a where
   lit :: Integer -> a
   add :: a -> a -> a
   mul :: a -> a -> a

instance Expr E.ExprT where
    lit = E.Lit
    add = E.Add
    mul = E.Mul

reify :: E.ExprT -> E.ExprT
reify = id

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (<= 0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where 
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax (max a b)
    mul (MinMax a) (MinMax b) = MinMax (min a b)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit n = Mod7 (mod n 7)
    add (Mod7 a) (Mod7 b) = Mod7 (mod (a + b) 7)
    mul (Mod7 a) (Mod7 b) = Mod7 (mod (a * b) 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7

-- Exercise 5
instance Expr S.Program where
    lit x = [S.PushI x]
    add x y = x ++ y ++ [S.Add]
    mul x y = x ++ y ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul


-- Exercise 6
class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)

instance  Expr VarExprT where
    lit = Lit
    add = Add
    mul = Mul

instance HasVars VarExprT where
    var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

maybeBinaryOp :: Num a => (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
maybeBinaryOp f (Just x) (Just y) = Just (x `f` y)
maybeBinaryOp f _ _ = Nothing

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit x = (\m -> Just x)
    add f g = (\m -> maybeBinaryOp (+) (f m) (g m))
    mul f g = (\m -> maybeBinaryOp (*) (f m) (g m))

type EvalVarMap = (M.Map String Integer -> Maybe Integer)

withVars :: [(String, Integer)]
            -> EvalVarMap
            -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
