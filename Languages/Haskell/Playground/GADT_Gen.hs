{-# LANGUAGE GADTs, NoMonoLocalBinds #-}

import Data.Kind

{---------------------------------------------------
Axiom
---------------------------------------------------}
data AxiomSkEsc :: Type -> Type -> Type where
  MkAxiomSkEsc1 :: forall a1 b1 . (a1 ~ b1) => b1 -> AxiomSkEsc a1 Bool
  MkAxiomSkEsc2 :: forall a1 a2 . ()        => a1 -> AxiomSkEsc a1 a2


attemptEsc = \ase -> case ase of
                       MkAxiomSkEsc1 b1 -> b1
                       MkAxiomSkEsc2 a  -> a

{---------------------------------------------------
Skolem as Part of Larger Type
---------------------------------------------------}
data SkEmbedded :: Type -> Type -> Type where
  -- a1 ~ Int -> b1 => a1 ~ Int -> b'1 @ n /\ (b1' @ n <-| b1 @ n+1)
  MkSkEmbedded1 :: forall a1 a2 b1 . (a1 ~ (Int -> b1),  a2 ~ Bool) =>
                   b1 -> SkEmbedded a1 a2 -- This can technically be [b1 -> SkEmbedded (Int -> b1) a2]
  MkSkEmbedded2 :: forall a1 a2    . () =>
                   a1 -> SkEmbedded a1 a2                   

skEmbedded1 = \gadt -> case gadt of
                         {- (2)
                         We are trying to solve
                            [ (a1 ~ (Int -> b1),  a2 ~ Bool) => a1 ~ (Int -> b1) ]
                         Intuitively, we use the local constraint to solve it
                         This seems to require [a1] to be unified with a type that
                         has higher level -}
                         MkSkEmbedded1 b1 -> \(_ :: Int) -> b1
                         {- (1)
                         Here we get [SkEmbedded p b -> p]
                         What is left is to type check the other branch -}
                         MkSkEmbedded2 a1 -> a1

{---------------------------------------------------
Skolem Equivalent to Bigger Type
---------------------------------------------------}
data SkEquivBig :: Type -> Type -> Type where
  MkSkEquivBig1 :: forall a1 a2 b1 . (b1 ~ (Int -> a1), a2 ~ Bool) => 
                   b1 -> SkEquivBig a1 a2
  MkSkEquivBig2 :: forall a1 a2 b1 . () =>
                   a1 -> SkEquivBig a1 a2

skEquivBig1 = \gadt -> case gadt of
                         MkSkEquivBig1 b1 -> b1
                         MkSkEquivBig2 a1 -> (\(_ :: Int) -> a1)

{---------------------------------------------------
Existential Not Used
---------------------------------------------------}
data MyType1 :: Type -> Type -> Type where
  MkMyType11 :: forall a1 a2 bf bg bmid . (bf ~ (a1 -> bmid), bg ~ (bmid -> a1), a2 ~ Bool) =>
                bf -> bg -> MyType1 a1 a2
  MkMyType12 :: forall a1 a2 . () =>
                a1 -> MyType1 a1 a2

existential1 :: MyType1 a1 a2 -> (a1 -> a1)
existential1 = \gadt -> case gadt of -- guess gadt :: MyType1 (alpha1 @ n) (alpha2 @ n)
                          --                                       V incorrect level, untouchable
                          MkMyType11 bf bg -> bg . bf -- (gamma' @ n -> alpha1 @ n) ~ (alpha1 @ n -> alpha1 @ n)
                          MkMyType12 a1 -> (\_ -> a1) -- beta @ n ~ (gamma @ n + 1 -> alpha1 @ n)

{---------------------------------------------------
Justify Quantifying Residual (Failed)
---------------------------------------------------}
data JustifyQuantifyResidual :: Type -> Type where
  MkJQR1 :: forall a1 b1 . (a1 ~ Bool) =>
            b1 -> JustifyQuantifyResidual a1
  MkJQR2 :: forall a1  . () =>
            () -> JustifyQuantifyResidual a1

justifyQuantifiedResidual t =
  case t of
    MkJQR1 b -> let f = \g -> not (g b)
                in ()
    MkJQR2 _ -> ()
