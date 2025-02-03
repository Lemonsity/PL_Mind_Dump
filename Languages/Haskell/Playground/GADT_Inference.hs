{-# LANGUAGE GADTs , DataKinds #-}

import Data.Kind

data T :: Type -> Type where
  TC1 :: Int -> T Bool -- (a ~ Bool) => Int -> T a
  TC2 :: [a] -> T a    -- ()         => [a] -> T a

f2 = \t -> case t of
             (TC1 n) -> n > 0
             (TC2 xs) -> null xs

f3 = \x -> case x of
             (TC1 n) -> n > 0
             (TC2 xs) -> head xs

data Expr :: Type -> Type where
  -- forall a . forall   . (Int ~ a) => Int -> Expr a 
  CNum :: Int -> Expr Int
  -- forall a . forall   . (Bool ~ a) => Bool -> Expr Bool
  CBool :: Bool -> Expr Bool 
  -- forall a . forall   . (Int ~ a) => Expr a -> Expr a -> Expr a
  CPlus :: Expr Int -> Expr Int -> Expr Int 
  -- forall a . forall b . (Bool ~ b) => Expr b -> Expr a -> Expr a -> Expr a
  CIf :: Expr Bool -> Expr a -> Expr a -> Expr a 

evalExpr :: Expr a -> a
evalExpr (CNum i) = i
evalExpr (CBool b) = b
evalExpr (CPlus en em) = (evalExpr en) + (evalExpr em)
evalExpr (CIf eb e1 e2) = if (evalExpr eb)
                          then (evalExpr e1)
                          else (evalExpr e2)

evalHalf (CNum i) = i
evalHalf (CBool b) = b
evalHalf (CPlus en em) = evalExpr en
evalHalf (CIf eb e1 e2) = if (evalExpr eb)
                          then (evalExpr e1)
                          else (evalExpr e2)

data R where
  RC :: forall a b . (a ~ b) => a -> b -> R

data S :: Type -> Type where
  SC :: forall a b . (a ~ b) => b -> S a

data Y :: Type -> Type where
  YC :: forall a b . (a ~ b) => a -> b -> Y a

f4 = \t -> case t of
             (YC a b) -> a

data P :: Type -> Type where
  PC :: forall a . P a

f5 = \t -> case t of
             PC -> 10

data Z :: Type -> Type where
  ZC :: forall a b1 b2 . (a ~ b1) => a -> b1 -> b2 -> Z a

-- f6 = \t -> case t of
--              (ZC x y z) -> if True then x else z
  
