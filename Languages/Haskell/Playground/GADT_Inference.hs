{-# LANGUAGE GADTs , NoMonoLocalBinds #-}

import Data.Kind

data T :: Type -> Type where
  TC1 :: Int -> T Bool -- (a ~ Bool) => Int -> T a
  TC2 :: [a] -> T a    -- ()         => [a] -> T a

f2 = \t -> case t of
             (TC1 n) -> n > 0
             (TC2 xs) -> null xs

f3 = \x -> case x of
             (TC1 n) -> n > 0
             (TC2 xs) -> (length xs) > 0

data Expr :: Type -> Type where
  -- forall a . forall   . (Int ~ a) => Int -> Expr a 
  CNum :: Int -> Expr Int
  -- forall a . forall   . (Bool ~ a) => Bool -> Expr Bool
  CBool :: Bool -> Expr Bool 
  -- forall a . forall   . (Int ~ a) => Expr a -> Expr a -> Expr a
  CPlus :: Expr Int -> Expr Int -> Expr Int 
  -- forall a . forall b . (Bool ~ b) => Expr b -> Expr a -> Expr a -> Expr a
  CIf :: Expr Bool -> Expr a -> Expr a -> Expr a 

{- Failed inference

This example will fall to infer in GHC 9.10.1 and HLS 2.9.0.1

May not be GADT related, but with recursion
-}
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


{---------------------------------------------------
GHC Doesn't Implment OutsideIn Exactly

The following example demonstrates GHC does not
implement OutsideIn to the details

Constraint generation should assign the following
types and constratins:
a := alpha
output of case := beta

[alpha, beta] (forall b . alpha ~ b >> alpha ~ beta)

Technically, OutsideIn cannot solve this constraint

But this is solved in GHC, likely because it noticed
the type of YC can be simplifed to

YC :: forall a . a -> a -> Y a
---------------------------------------------------}
data ReductionHappens :: Type -> Type where
  MkReductionHappens :: forall a b . (a ~ b) => a -> b -> ReductionHappens a

-- f4 :: Y p -> p
f4 = \t -> case t of
             (MkReductionHappens a b) -> a

{---------------------------------------------------
When Existential Type is not used
---------------------------------------------------}
data NoExistential :: Type -> Type where
  MkNoExistential :: forall a . NoExistential a

-- f5 :: P a -> Integer
f5 = \t -> case t of
             MkNoExistential -> 10

{---------------------------------------------------
Existential Types should not leak out
---------------------------------------------------}
data DontLeakExist :: Type -> Type where
  MkDontLeakExist :: forall a b . a -> b -> DontLeakExist a

-- The return type, aka the type of [case] expression,
-- is assigned a type variable [p]
-- The type of [y] is assigned a variable [b]
-- These two are both considered "rigit"
-- The former from being "untouchable"
-- The latter from being "skolem"
-- Thus unification fails, and no inference is done

-- f6' = \t -> case t of
--               MkDontLeakExist x y -> y

{---------------------------------------------------
Testing Local Constraints Capability
---------------------------------------------------}
data SecretCons :: Type -> Type where
  CSecretCons1 :: forall a b . (a ~ [b]) => b -> a -> SecretCons a
  CSecretCons2 :: forall a   . a -> SecretCons a

fSecretCons = \t -> case t of
                      CSecretCons1 x xs -> x : xs
                      CSecretCons2 x -> x

{---------------------------------------------------
Pushing it with nested cases
---------------------------------------------------}

data HideSame :: Type -> Type where
  MkHideSame :: forall a b . (a ~ b) => a -> b -> HideSame a

-- GHC probably does some simplification
usingHideSame = \t -> case t of
                        MkHideSame a b -> case MkHideSame b b of
                                            MkHideSame a' b' -> [b, b']

{---------------------------------------------------
Bogus Local Constraints
---------------------------------------------------}
data MyU = MyU
data MyB = MyT | MyF

myNot = \myb -> case myb of
                  MyT -> MyF
                  MyF -> MyT

data Bogus where
  MkBogus :: ( Bool ~ () ) => Bool -> () -> Bogus

-- The bogus local constaint is used to type check the body
useBogus bogus = case bogus of
                   MkBogus b unit -> not ()

-- The bogus local constraint is no use to check the branch body
-- Thus the type error is raise in the branch body too
useBogusAgain bogus = case bogus of
                        MkBogus b unit -> myNot MyU

{---------------------------------------------------
Axiom
---------------------------------------------------}
data Axiom :: Type -> Type -> Type where
  MkAxiom :: forall a1 a2 . (a1 ~ (Int -> a2)) => a1 -> a2 -> Axiom a1 a2

data AxiomSkEsc :: Type -> Type -> Type where
  MkAxiomSkEsc1 :: forall a1 b1 . (a1 ~ b1) => b1 -> AxiomSkEsc a1 Bool
  MkAxiomSkEsc2 :: forall a     .              a -> AxiomSkEsc a a


attemptEsc = \ase -> case ase of
                       MkAxiomSkEsc1 b1 -> b1
                       MkAxiomSkEsc2 a  -> a



-- Alternative example
data Z :: Type -> Type where
  ZC :: forall a b1 b2 . (a ~ b1) => a -> b1 -> b2 -> Z a

-- Fail to infer, because b2 is an existential type variable
-- f6 = \t -> case t of
--              (ZC x y z) -> if True then x else z
  

                       


main :: IO ()
main = return ()
