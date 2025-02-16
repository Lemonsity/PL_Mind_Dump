{-# LANGUAGE GADTs , DataKinds #-}

import Data.Kind

data Expr :: Type -> Type where
  -- forall a . forall   . (Int ~ a) => Int -> Expr a 
  CNum :: Int -> Expr Int
  -- forall a . forall   . (Bool ~ a) => Bool -> Expr Bool
  CBool :: Bool -> Expr Bool 
  -- forall a . forall   . (Int ~ a) => Expr a -> Expr a -> Expr a
  CPlus :: Expr Int -> Expr Int -> Expr Int 
  -- forall a . forall b . (Bool ~ b) => Expr b -> Expr a -> Expr a -> Expr a
  CIf :: Expr Bool -> Expr a -> Expr a -> Expr a 

{- Inaccessible RHS

Tested in GHC 9.10.1, HLS 2.9.0.1

There are two main issues with the following code:
1. Type inference fails, likely caused by recursion, and some constraint solving
2. In Emacs, turning on eglot, it will underline the `CNum` and `CPlus` case with
   "Inaccessible RHS"

Apparently, exhaustiveness issues can trigger the #2 error, but I don't know why
-}
-- evalExpr (CNum i) = i
-- evalExpr (CBool b) = b
-- evalExpr (CPlus en em) = (evalExpr en) + (evalExpr em)
-- evalExpr (CIf eb e1 e2) = if (evalExpr eb)
--                           then (evalExpr e1)
--                           else (evalExpr e2)
