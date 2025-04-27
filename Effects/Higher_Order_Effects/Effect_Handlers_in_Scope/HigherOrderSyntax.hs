{-# LANGUAGE
ViewPatterns,
PatternSynonyms,
MultiParamTypeClasses,
UndecidableInstances,
OverlappingInstances,
NamedFieldPuns #-}

module HigherOrderSyntax where

import TraditionalAndScoped

import Control.Monad
import Data.Kind

import Debug.Trace

{- Effect Handlers in Scope

Here we type up the code from "Effect Handlers in Scope"
by Nicolas Wu, Tom Schrijvers, and Ralf Hinze [2014]

This file covers section 10 and onward

This code is written in GHC 9.10.1
-}

{--------------------------------------------------------
Higher-order Syntax Typeclasses
--------------------------------------------------------}

type f ⇝ g = forall x . f x -> g x -- Natural transformation between two functors

class HFunctor (h :: (Type -> Type) -> Type -> Type) where
  hmap :: (Functor f, Functor g) => (f ⇝ g) -> (h f ⇝ h g)

class HFunctor sig => Syntax sig where
  -- If [m] is some effectful computation context
  -- then [sig m] is also some effectful computation context
  emap :: (Monad m) => (m a -> m b) -> (sig m a -> sig m b)

  handle :: (Monad m, Monad n, Functor c) =>
            c () ->
            (forall x . c (m x) -> n (c x)) ->
            sig m a -> sig n (c a)

{--------------------------------------------------------
Higher-Order Programs

[Prog] is Still a Monad
--------------------------------------------------------}

data Prog (sig :: (Type -> Type) -> Type -> Type) (a :: Type)
  = Return a
  | Op (sig (Prog sig) a)

instance (Syntax sig) => Functor (Prog sig) where
  fmap = liftM

instance (Syntax sig) => Applicative (Prog sig) where
  pure v = Return v
  (<*>) = ap

instance Syntax sig => Monad (Prog sig) where
  return = pure
  
  Return a >>= prog = prog a
  Op op >>= prog = Op (emap (>>= prog) op)            

{--------------------------------------------------------
The Relationship Between the Pieces

Here we have some concepts:
(1) Functor m => Functor (sig m)
(2) hmap :: (Functor f, Functor g) => (f ⇝ g) -> (h f ⇝ h g)
(3) HFunctor sig => Syntax sig               (a)
                 => Functor (Prog sig)       
                 => Applicative (Prog sig)   
                 => Monad (Prog sig)         (b)

About (1):
  The [Functor m] may be considered as some effects the
  new effect [sig] is depended upon.
  In the example, [sig = HExc e], and [HExc e m] means:
  "Exception can happen in some other effectful context m"

About (2):
  If there is some transformation between two computation
  context [f, g] that the higher-order effect is depended
  upon,
  Then it is also possible to transform the higher-order
  effects [h f, h g]

About (3a) [hmap]:
  If the [hmap] from (2) justifies the capability of
  transforming the evaluation context,
  Then the [emap] from (3a) justifies the change of
  computation return type.
  This may also be why [emap] appears in the definition
  of [>>=] later on

About (3a) [handle]:
  We first understand the point of [c] in the type

  In some sense, every operation can denote a scope
  on its own.
  At the beginning of a scope, we want to take a
  snapshot of the current status of the computation.

  This status can come in many forms:
  - Internal state
  - Success / Failure status
  - Accumulated output

  [c] the whatever functor that can be used to represent
  this status. The paper also refer to this surrounding
  status as the "context"

  [c ()]:
    Only supply the external status context
  [forall x . c (m x) -> n (c x)]:
    Think about the remaining non-processed computation
    tree.
    We will put the said tree into the status context
    (Optionally modifying the context). The tree in
    context is the [c (m x)]
    We are expecting a way to transform this "tree in
    context" into a tree with different operations.
    Except the leaf nodes will be wrapped in context
    The context here may be updated

About (3b)

--------------------------------------------------------}

{--------------------------------------------------------
Higher-order Syntax Example: [HExc]
--------------------------------------------------------}

data HExc (e :: Type) (m :: Type -> Type) (a :: Type)
  = Throw' e
  | forall x . Catch' { p :: (m x)        -- Recoverable computation
                      , h :: (e -> m x)   -- Handler for a throw
                      , k :: (x -> m a) } -- Continuation for after the catch

-- [m] is a placeholder for some computation context

{- [HExc] instantiated is a functor -}
instance Functor m => Functor (HExc e m) where
  fmap f (Throw' e) = Throw' e
  fmap f (Catch' p h k) = Catch' p h (fmap f . k)

{- [HExc] can induce transformation -}
instance HFunctor (HExc e) where
  hmap (t)                          (Throw' e)     = Throw' e
  hmap (t :: forall x . f x -> g x) (Catch' p h k) = Catch' (t p) (t . h) (t . k)

instance Syntax (HExc e) where
  -- [HExc] behaves somewhat like normal syntax
  emap f (Throw' e) = Throw' e
  emap f (Catch' p h k) = Catch' p h (f . k)

  handle c hdl (Throw' e) = Throw' e
  handle c hdl (Catch' p h k) =
    Catch' { p = hdl (fmap (const p) c)
           , h = \e -> hdl (fmap (const (h e)) c)
           , k = hdl . fmap k }

pattern Throw e <- (project -> Just (Throw' e))

throw :: (HExc e ⊂ sig) => e -> Prog sig a
throw e = inject (Throw' e)

pattern Catch p h k <- (project -> Just (Catch' p h k))

catch :: (HExc e ⊂ sig) =>
         Prog sig a -> (e -> Prog sig a) -> Prog sig a
catch p h = inject (Catch' p h return)

{--------------------------------------------------------
Some Identities

emap id = id
(emap f) . (emap g) = emap (f . g)

fmap = emap . fmap
 ^
 |
This is the functor mapping for [sig m]
--------------------------------------------------------}

{--------------------------------------------------------
Higher-Order Infrastructure
--------------------------------------------------------}

class (Syntax sub, Syntax sup) => sub ⊂ sup where
  inj :: sub m a -> sup m a
  prj :: sup m a -> Maybe (sub m a)

{- Combining Signatures -}
data (sig1 + sig2) (m :: Type -> Type) (a :: Type)
  = Inl (sig1 m a)
  | Inr (sig2 m a)

instance (HFunctor sig1, HFunctor sig2) =>
         HFunctor (sig1 + sig2) where
  hmap t (Inl op) = Inl (hmap t op)
  hmap t (Inr op) = Inr (hmap t op)

instance (Syntax sig1, Syntax sig2) =>
         Syntax (sig1 + sig2) where
  emap f (Inl op) = Inl (emap f op)
  emap f (Inr op) = Inr (emap f op)

  handle c hdl (Inl op) = Inl (handle c hdl op)
  handle c hdl (Inr op) = Inr (handle c hdl op)

{- Once we have shown coproduct of syntax is still a
syntax, the monadic property of [Prog] can be
derived from the default implementation -}

{- Detect Signature in Set -}
instance (Syntax sig1, Syntax sig2) =>
         (sig1 ⊂ (sig1 + sig2)) where
  inj = Inl
  prj (Inl ma) = Just ma
  prj _ = Nothing

instance (Syntax sig1, sig ⊂ sig2) =>
         sig ⊂ (sig1 + sig2) where
  inj = Inr . inj
  prj (Inr ga) = prj ga
  prj _ = Nothing

-- Hide the actual operation
inject :: (sub ⊂ sup) => sub (Prog sup) a -> Prog sup a
inject = Op . inj

-- Extract the top-level operation (if it is indeed from sub)
project :: (sub ⊂ sup) => Prog sup a -> Maybe (sub (Prog sup) a)
project (Op s) = prj s
project _ = Nothing

{--------------------------------------------------------
Reuse First-Order Signature
--------------------------------------------------------}

newtype (Lift (sig :: Type -> Type)) (m :: Type -> Type) (a :: Type)
  = Lift (sig (m a))

instance Functor sig => HFunctor (Lift sig) where
  hmap t (Lift op) = Lift (fmap t op)

instance Functor sig => Syntax (Lift sig) where
  emap f (Lift op) = Lift (fmap f op)
  handle c hdl (Lift op) = Lift (fmap (\p -> hdl (fmap (const p) c)) op)

{- [handle] in [Lift sig]

First-Order operation signatures are at least functors

A top-level first-order operation has a computation tree
[m a] as its child. We place the child tree in a context
[c]

The outer-most [fmap] is used because we cannot inspect
the top-level operation
-}
  
-- Pure Computation
type HVoid = Lift Void

run :: Prog HVoid a -> a
run (Return x) = x
run _ = error ("There is no constructor for Void\n"
               ++ "It is impossible to create Op (Lift _Void_)")

pattern Other s = Op (Inr s)        

-- Higher-Order State
type (HState s) = Lift (State s)

pattern Get k <- (project -> Just (Lift (Get' k)))
pattern Put s k <- (project -> Just (Lift (Put' s k)))

get :: (HState s ⊂ sig) => Prog sig s
get = inject (Lift (Get' return))

put :: (HState s ⊂ sig) => s -> Prog sig ()
put s = inject (Lift (Put' s (return ())))


{--------------------------------------------------------
Higher-Order Handler

Using [HState] as an example
--------------------------------------------------------}

runState :: (Syntax sig) =>
            s -> Prog (HState s + sig) a -> Prog sig (s, a)
runState s (Return a) = return (s, a)
runState s (Get k) = runState s (k s)
runState s (Put s' k) = runState s' k
runState s (Other op) = Op (handle (s, ()) (uncurry runState) op)

runExc :: (Syntax sig) =>
          Prog (HExc e + sig) a -> Prog sig (Either e a)
runExc (Return x) = return (Right x)
runExc (Throw e) = return (Left e)
runExc (Catch p h k) =
  do { r <- runExc p
     ; case r of
         Left e -> do { r <- runExc (h e)
                      ; case r of
                          Left e -> return (Left e)
                          Right x -> runExc (k x) }
         Right x -> runExc (k x) }
runExc (Other op) =
  Op (handle (Right ()) hdl op) where
  hdl :: (Syntax sig) =>
         (forall x . Either e (Prog (HExc e + sig) a) -> Prog sig (Either e a))
  hdl = either (return . Left) runExc


{--------------------------------------------------------
Higher-Order Effect In Action
--------------------------------------------------------}

decr :: (HState Int ⊂ sig, HExc () ⊂ sig) => Prog sig ()
decr = do { x <- get
              ; if x > (0 :: Int) then put (pred x) else throw () }

tripleDecr :: (HState Int ⊂ sig, HExc () ⊂ sig) => Prog sig ()
tripleDecr = decr >> catch (decr >> decr) return

runTriple = (run
             . runExc
             . runState (2 :: Int))
            (tripleDecr :: Prog (HState Int + (HExc () + HVoid)) ())

decrChar :: (HState Int ⊂ sig, HExc [Char] ⊂ sig) => Prog sig ()
decrChar = do { x <- get
              ; if x > (0 :: Int) then put (pred x) else throw "Already 0" }

tripleDecrChar :: (HState Int ⊂ sig, HExc [Char] ⊂ sig) => Prog sig [Char]
tripleDecrChar = decrChar >> catch (decrChar >> decrChar >> return "Done") return

runTripleChar = (run
                 . runExc
                 . runState (2 :: Int))
                (tripleDecrChar :: Prog (HState Int + (HExc [Char] + HVoid)) [Char])

decrChar121 :: (HState Int ⊂ sig, HExc [Char] ⊂ sig) => Prog sig [Char]
decrChar121 = do { decrChar
                 ; catch
                   (decrChar >> decrChar >> decrChar >> return "Catch Passed")
                   (\errMsg -> return ("Catch Failed: " ++ errMsg))
                 ; decrChar
                 ; return "Done" }

decrChar121Checkpoint :: (HState Int ⊂ sig, HExc [Char] ⊂ sig) => Prog sig [Char]
decrChar121Checkpoint = do { decrChar
                           ; catch
                             (decrChar >> decrChar >> decrChar >> return "Catch Passed")
                             (\errMsg -> return ("Catch Failed: " ++ errMsg))
                           ; x <- get
                           -- Here you will see that the state is indeed global
                           ; trace (show (x :: Int)) (return ()) 
                           ; decrChar
                           ; return "Done" }

runDecrChar121Local = (run
                       . runExc
                       . runState (3 :: Int))
                      (decrChar121 :: Prog (HState Int + (HExc [Char] + HVoid)) [Char])

runDecrChar121Global = (run
                        . runState (3 :: Int)
                        . runExc)
                       (decrChar121 :: Prog (HExc [Char] + (HState Int + HVoid)) [Char])

runDecrChar121LocalCheck = (run
                            . runExc
                            . runState (3 :: Int))
                           (decrChar121Checkpoint :: Prog (HState Int + (HExc [Char] + HVoid)) [Char])

runDecrChar121GlobalCheck = (run
                             . runState (3 :: Int)
                             . runExc)
                            (decrChar121Checkpoint :: Prog (HExc [Char] + (HState Int + HVoid)) [Char])
