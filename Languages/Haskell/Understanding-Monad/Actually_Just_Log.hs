{- Actually Just Log

The idea came to me while I was working on a
constraint rewrite algorithm

The algorithm should take a big constraint as
an input, manipulate it, and output a constraint
as an output.

But we might also discard some constratins,
in the sense they we don't even care if it has
a solution

-}

import Control.Monad

newtype JustLog discard constaints = JustLog (constaints, discard)

instance (Monoid discard) => Functor (JustLog discard) where
  fmap = liftM

instance (Monoid discard) =>  Applicative (JustLog discard) where
  pure = return 
  (<*>) = ap

instance (Monoid discard) => Monad (JustLog discard) where
  return :: constaints -> JustLog discard constaints
  return x = JustLog (x, mempty)

  JustLog (constaints, discard) >>= f = let JustLog (newConstaints, newDiscard) = f constaints
                             in JustLog (newConstaints, discard <> newDiscard)
