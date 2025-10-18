{-# LANGUAGE
ViewPatterns,
PatternSynonyms,
MultiParamTypeClasses,
UndecidableInstances,
OverlappingInstances #-}

module TraditionalAndScoped(
  State(Get', Put'),
  Void) where

import Control.Monad
import Data.Kind

import Debug.Trace


{- Effect Handlers in Scope

Here we type up the code from "Effect Handlers in Scope"
by Nicolas. Wu, Tom Schrijvers, and Ralf Hinze [2014]

This file covers section 1 - 9

This code is written in GHC 9.10.1
-}

{--------------------------------------------------------
Effectful Program Free Monad
--------------------------------------------------------}
data Prog (sig :: Type -> Type) (a :: Type)
  = Return a
  | Op (sig (Prog sig a))

instance (Functor sig) => Functor (Prog sig) where
  fmap = liftM

instance (Functor sig) => Applicative (Prog sig) where
  pure v = Return v
  (<*>) = ap

instance (Functor sig) => Monad (Prog sig) where
  return = pure
  (Return v) >>= prog = prog v
  (Op op) >>= prog = Op (fmap (>>= prog) op)

{--------------------------------------------------------
Combining Signatures (Effect Collections)
--------------------------------------------------------}
data (sig1 + sig2) cnt
  = Inl (sig1 cnt)
  | Inr (sig2 cnt)

-- Haskell could not deduce coproduct is a functor
instance (Functor sig1, Functor sig2) =>
         Functor (sig1 + sig2) where
  fmap f (Inl v) = Inl (fmap f v)
  fmap f (Inr v) = Inr (fmap f v)

class (Functor sub, Functor sup) => sub ⊂ sup where
  inj :: sub a -> sup a
  -- [prj]
  -- [sub] is a subset of [sup]. It is always possible to
  -- inject a program that only uses the effects from [sub]
  -- into a program that uses effects from [sup]
  -- However, a program using effects from [sup] may have a
  -- top-level operation call that is not from [sub]
  -- In which [prj] will fail

  -- In essense, the success/failure state indicate whether
  -- the top-level operation is from [sub]
  prj :: sup a -> Maybe (sub a)

instance (Functor sig1, Functor sig2) =>
         (sig1 ⊂ (sig1 + sig2)) where
  inj = Inl
  prj (Inl fa) = Just fa
  prj _ = Nothing

instance (Functor sig1, sig ⊂ sig2) =>
         sig ⊂ (sig1 + sig2) where
  inj = Inr . inj
  prj (Inr ga) = prj ga
  prj _ = Nothing

-- Hide the actual operation
inject :: (sub ⊂ sup) => sub (Prog sup a) -> Prog sup a
inject = Op . inj

-- Extract the top-level operation (if it is indeed from sub)
project :: (sub ⊂ sup) => Prog sup a -> Maybe (sub (Prog sup a))
project (Op s) = prj s
project _ = Nothing

{--------------------------------------------------------
Pattern definitions for easier handler def
--------------------------------------------------------}
data Nondet cnt
  = Fail'
  | Branch' cnt cnt

instance Functor Nondet where
  fmap f Fail' = Fail'
  fmap f (Branch' p q) = Branch' (f p) (f q)

{- Pattern definition
[project -> Just Fail']:
  This part uses ViewPattern. It will first run [project]
  on a term that wants to be matched
  And this pattern is triggered when the result of applying
  [project] is [Just Fail']

[pattern Fail <- ...]
  The RHS of the arrow is a pattern definition
  This pattern is now called [Fail] -}

pattern Fail <- (project -> Just Fail')

-- Operation call?
fail :: (Nondet ⊂ sig) => Prog sig a
fail = inject Fail'


pattern Branch p q <- (project -> Just (Branch' p q))

branching :: (Nondet ⊂ sig) => Prog sig a -> Prog sig a -> Prog sig a
branching p q = inject (Branch' p q)

{--------------------------------------------------------
Representation of Pure Computation
Representation of Remaining Computation
--------------------------------------------------------}
data Void cnt

instance Functor Void where
  fmap = error "Calling [fmap] on term with type [Void], should not be possible"

-- Because there is no constructor for [Void]
-- It is impossible to create [Op (Void ...)]
run :: Prog Void a -> a
run (Return x) = x

-- This does make an assumption,
-- that all signature are in the form of: E1 + ... + En + Void]
pattern Other s = Op (Inr s)
--            ^
--            |
--        (sig2 cnt)

{--------------------------------------------------------
Handler for Nondeterminism
--------------------------------------------------------}
solutions :: (Functor sig) => Prog (Nondet + sig) a -> Prog sig [a]
solutions (Return a) = return [a]
solutions (Fail) = return []
solutions (Branch p q) = liftM2 (++) (solutions p) (solutions q)
solutions (Other op) = Op (fmap solutions op)

-- op :: sig (Prog (Nondet + sig) a)
-- solutions :: Prog (Nondet + sig) a -> Prog sig [a]
-- fmap solutions :: sig (Prog (Nondet + sig) a) -> sig (Prog sig [a])

allsols :: Prog (Nondet + Void) a -> [a]
allsols = run . solutions

{--------------------------------------------------------
State Effect
--------------------------------------------------------}
data State s cnt
  = Get' (s -> cnt)
  | Put' s cnt

instance Functor (State s) where
  fmap f (Get' k) = Get' (\s -> f (k s))
  fmap f (Put' s k) = Put' s (f k)

pattern Get k <- (project -> Just (Get' k))

get :: (State s ⊂ sig) => Prog sig s
get = inject (Get' return) -- the use of return is pretty genius

pattern Put s k <- (project -> Just (Put' s k))

put :: (State s ⊂ sig) => s -> Prog sig ()
put s = inject (Put' s (return ()))

runState :: Functor sig =>
            s -> Prog (State s + sig) a -> Prog sig (s, a)
runState s (Return a) = return (s, a)
runState s (Get k) = runState s (k s)
runState s (Put s' k) = runState s' k
runState s (Other op) = Op (fmap (runState s) op)

{--------------------------------------------------------
Mixing Effects
--------------------------------------------------------}
-- WARNING: effect collection is left assoc by default

-- Each branch get a different universe,
-- No shared state
-- WARNING: Associativity enforcement is needed
runLocal :: (Functor sig) =>
            s -> Prog (State s + (Nondet + sig)) a -> Prog sig [(s, a)]
runLocal s = solutions . (runState s)

-- Shared state across branches
runGlobal :: (Functor sig) =>
             s -> Prog (Nondet + (State s + sig)) a -> Prog sig (s, [a])
runGlobal s = (runState s) . solutions

incr :: (State Int ⊂ sig) => Prog sig ()
incr = get >>= (put . (succ :: Int -> Int))

choices :: (Nondet ⊂ sig, State Int ⊂ sig) =>
           Prog sig a -> Prog sig a
choices (Return a) = return a
choices (Fail) = TraditionalAndScoped.fail
choices (Branch p q) = incr >> ((choices p) `branching` (choices q))
choices (Op op) = Op (fmap choices op)

select :: (Nondet ⊂ sig) => [a] -> Prog sig a
select [] = TraditionalAndScoped.fail
select (x : xs) = (return x) `branching` (select xs)

knapsack :: (Nondet ⊂ sig) => Int -> [Int] -> Prog sig [Int]
knapsack w vs
  | w < 0 = TraditionalAndScoped.fail
  | w == 0 = return []
  | w > 0 = do { v <- select vs
               ; vs' <- knapsack (w - v) vs
               ; return (v : vs') }

knapsack1 = (run . (runGlobal (0 :: Int)) . choices) (knapsack 3 [3, 2, 1])
knapsack2 = (run . (runLocal (0 :: Int)) . choices) (knapsack 3 [3, 2, 1])

{--------------------------------------------------------
Cut and Call
--------------------------------------------------------}
data Cut cnt = Cutfail'

instance Functor Cut where
  fmap _ _ = Cutfail'

pattern Cutfail <- (project -> Just Cutfail')

cutfail :: (Cut ⊂ sig) => Prog sig a
cutfail = inject Cutfail'

call :: (Nondet ⊂ sig) => Prog (Cut + sig) a -> Prog sig a
call p = go p TraditionalAndScoped.fail where
  go :: (Nondet ⊂ sig) =>
        Prog (Cut + sig) a -> Prog sig a -> Prog sig a
  go (Return a) q = (return a) `branching` (q)
  go (Fail) q = q
  go (Cutfail) _ = TraditionalAndScoped.fail
  go (Branch p1 p2) q = (go p1 (go p2 q))
  go (Other op) q = Op (fmap (flip go q) op)

skip :: Monad m => m ()
skip = return ()

cut :: (Nondet ⊂ sig, Cut ⊂ sig) => Prog sig ()
cut = skip `branching` cutfail

{- Intuition of [cut, call, go]
Consider if we bind [cut]  the program to a program
At this point our program ends with a branching of
     ...
    /   \
   ()  cutfail
If we bind this branching computation with more
computations, by the defintion of [Functor Nondet],
the further computation will be appended to [()] and [fail]

Now go back to consider the implementation of [go]
[go] in essense traverse a nondet computation tree
by doing a DFS

So once it hit fail, a bunch of the tree will no longer
be explored

[call] is just a wrapper for [go] that append a [fail] to
the DFS result -}

once :: (Nondet ⊂ sig) => Prog (Cut + sig) b -> Prog sig b
once p = call (do { x <- p
                  ; cut
                  ; return x })

{- Intuitition of [once]
We expand the [do] syntax, and the [cut] content

We recall [>>=] for Nondet append computation to the end
Thus we see each [Return] leaf in the computation tree
go through the following transformation:

     ...                 ...
      |                   |
   Return x           Op Branch
                        /   \
                       ()   Cutfail'
                      /
                  Return x

Relate back to [call / go] DFS traverse the computation
tree, we can see what it means by committing to one branch
-}
once' p = call (p >>=
                \x -> (skip `branching` cutfail) >>=
                \_ -> return x)

{- Thoughts

I also think this functionality is only achieved by using
the laziness of Haskell

Otherwise, any finite computation tree with at least one
[Cutfail'] in it will be pruned away entirely

What we hope is the [solutions] collect some soluitons
before it gets to the branch that have a [Cutfail']
-}

takeOneChoice = (run . solutions . once) (knapsack 3 [3, 2, 1])

{--------------------------------------------------------
Parser
--------------------------------------------------------}

data Symbol cnt = Symbol' Char (Char -> cnt)

instance Functor Symbol where
  fmap f (Symbol' c k) = Symbol' c (f . k)

pattern Symbol c k <- (project -> Just (Symbol' c k))

symbol :: (Symbol ⊂ sig) => Char -> Prog sig Char
symbol c = inject (Symbol' c return)

digit :: (Nondet ⊂ sig, Symbol ⊂ sig) =>
         Prog sig Char
digit = foldr branching TraditionalAndScoped.fail (fmap symbol ['0'..'9'])

many, many1, some :: (Nondet ⊂ sig) => Prog sig a -> Prog sig [a]

many p = many1 p `branching` return []

many1 p = do { a <- p
             ; as <- many p
             ; return (a : as) }

some p = do { a <- p -- [some] is another used name for "at least 1"
             ; as <- many p
             ; return (a : as) }

parse :: (Nondet ⊂ sig) =>
         [Char] -> Prog (Symbol + sig) a -> Prog sig a
parse [] (Return a) = return a
parse (x : xs) (Return a) = TraditionalAndScoped.fail
parse [] (Symbol c k) = TraditionalAndScoped.fail
parse (x : xs) (Symbol c k) | x == c = parse xs (k c)
                            | otherwise = TraditionalAndScoped.fail
parse xs (Other op) = Op (fmap (parse xs) op)

expr :: (Nondet ⊂ sig, Symbol ⊂ sig) => Prog sig Int
expr = (do i <- term ; symbol '+' ; j <- expr ; return (i + j))
       `branching`
       (do i <- term ; return i)

term :: (Nondet ⊂ sig, Symbol ⊂ sig) => Prog sig Int
term = (do i <- factor ; symbol '*' ; j <- term ; return (i * j))
       `branching`
       (do i <- factor ; return i)

factor :: (Nondet ⊂ sig, Symbol ⊂ sig) => Prog sig Int
factor = (do ds <- many1 digit ; return (read ds))
         `branching`
         (do symbol '(' ; i <- expr ; symbol ')' ; return i)

{- The Essense of Parsing

In reality, [expr] represents an infinite computation tree,

[parse "2+8*5"] takes the infinite computation tree, narrowing it down to a substree

[allsol] collects answers at the leaves -}

eval285 = (allsols . parse "2+8*5") expr -- [42]

-- The fact the result is a singleton means only one branch succeeds

{--------------------------------------------------------
Refactoring Causes Issues
--------------------------------------------------------}

-- Both branches need to parse at least one term
-- collect like terms
expr1 :: (Nondet ⊂ sig, Symbol ⊂ sig) => Prog sig Int
expr1 = do { i <- term
           ; ((do { symbol '+' ; j <- expr1 ; return (i + j) } )
              `branching`
              (do { return i })) }

{- The Motive Behind Problematic Code

We need to justify shortcoming of traditional effect handlers
The following optimization is not unreasonable thing to write

If there is a '+' following the first term, then we must commit
to finding another term. There is no way of falling back to
a term with no operator -}
exprBad :: (Nondet ⊂ sig, Symbol ⊂ sig) => Prog sig Int
exprBad = do { i <- term
             ; call ((do { symbol '+' ; cut ; j <- exprBad ; return (i + j) } )
                     `branching`
                     (do { return i })) }
-- The ^^^ function is called [expr2] in the paper

badParsing = (allsols . parse "1") exprBad

{- Issues
Consider running [call] on [exprBad]

The [go] in [call] will traverse the computation tree
But during the process, we always have to parse a '+' first

This causes issue when the term to parse is just a number -}


{- Simple solution does not work

Pulling [cut] out does not change the fact [symbol '+'] will get
pushed out of the [call] handler
Will once again run into the issue of expecting a "+"
-}
exprBadAlt :: (Nondet ⊂ sig, Symbol ⊂ sig) => Prog sig Int
exprBadAlt = call ((do { i <- term ; symbol '+' ; cut ; j <- exprBadAlt ; return (i + j) } )
                   `branching`
                   (do { i <- term ; return i }))

-- The following does not work for the same reason as [exprBadAlt]
exprBadRev :: (Nondet ⊂ sig, Symbol ⊂ sig) => Prog sig Int
exprBadRev = do { i <- term
                ; call ((do return i)
                        `branching`
                        (do symbol '+' ; j <- exprBadRev ; return (i + j))) }

badParsingAlt = (allsols . parse "1+10") exprBadRev
badParsingAlt' = (allsols . parse "10") exprBadRev

{- Non-solution
We could consider going to the extreme, where we do not use
the [call] handler within [expr]. Instead, we put [call]
outside of [parse]

However, that would make the [call] handler handler a larger
scope, which is not what we want -}

{--------------------------------------------------------
Scoping Handler

Borrowing notations from papers on Eff,
the type of handler [call] is roughly the following:

A ! { Cut, Nondet, e } => A ! { Nondet, e }

With traditional effect handler, whatever program the
handler [call] is applied to is the scope of the handler

Another property with traditional handler is that if the
handler encounters an operation it cannot handled, the
handler is pushed into the continuation.
This "pushing in" process effectively changes the scope
of the handler.

The issue we ran into is that when [go] in [call]
encounters the following:

go (do symbol '+' ; p1) (go p2 fail)

It sees the operation [symbol], doesn't know how to handle
it, so forward it outside:

do symbol '+' ; (go p1 (go p2 fail))

Binding this program to the end of [i <- term] gives the
semantic of:

"Every term should be followed by a symbol '+'"

Which is incorrect
--------------------------------------------------------}

{--------------------------------------------------------
What is the Problem

Intuitively, the semantic we are trying to achieve is the
following:

- Try to parse a '+'
- If failed, backtrack to before parsing '+', and try the
  alternative

The problem with traditional handler is when a handler is:
1. Top level operation dictate the whether a handler is
   pushed in
2. There is no mechanism for a handler to be "pulled back
   out"
--------------------------------------------------------}

{--------------------------------------------------------
Scoped Syntax
--------------------------------------------------------}

-- Explicit scope
data Call cnt
  = BCall' cnt
  | ECall' cnt

pattern BCall p <- (project -> Just (BCall' p))
pattern ECall p <- (project -> Just (ECall' p))

instance Functor Call where
  fmap f (BCall' cnt) = BCall' $ f cnt
  fmap f (ECall' cnt) = ECall' $ f cnt

beginContent :: (Call ⊂ sig) => Call (Prog sig ())
beginContent = BCall' (return ())

beginContent' = BCall' (Return ()) -- Equiv to ^^^

endContent :: (Call ⊂ sig) => Call (Prog sig ())
endContent = ECall' (return ())

endContent' = ECall' (Return ()) -- Equiv to ^^^

begin :: (Call ⊂ sig) => Prog sig ()
begin = inject beginContent

end :: (Call ⊂ sig) => Prog sig ()
end = inject endContent

call' :: (Call ⊂ sig) => Prog sig a -> Prog sig a
call' p = do { begin
             ; x <- p
             ; end
             ; return x }

-- I wonder if we could just use [inject]
upcast :: (Functor f, Functor sig) =>
          Prog sig a -> Prog (f + sig) a
upcast (Return x) = return x
upcast (Op op) = Op (Inr (fmap upcast op))

-- WARNING: Associativity enforcement needed
bcall :: (Nondet ⊂ sig) =>
         Prog (Call + (Cut + sig)) a -> Prog (Cut + sig) a
bcall (Return a) = return a
bcall (BCall p) = upcast (call (ecall p)) >>= bcall
bcall (ECall p) = error "Mismatched ECall!"
bcall (Other op) = Op (fmap bcall op)

ecall :: (Nondet ⊂ sig) =>
         Prog (Call + (Cut + sig)) a ->
         Prog (Cut + sig) (Prog (Call + (Cut + sig)) a)
ecall (Return a) = return (Return a)
ecall (BCall p) = upcast (call (ecall p)) >>= ecall
ecall (ECall p) = return p
ecall (Other op) = Op (fmap ecall op)

runCut :: (Nondet ⊂ sig) =>
          Prog (Call + (Cut + sig)) a -> Prog sig a
runCut p = call (bcall p)

{- Intuition of Definitions

There is only one order of the handlers that make sense:
  Run [Symbol] before [Call]
Switching the order will always result in the [symbol '+']
operation escaping [Call] and lead to undesired behaviour

Thus, the remaining goal is to ensure [call] only does
pruning within a scope

[call']:
  Wraps a computation in a scope, with explicit operations
  acting as delimiters

I believe the key design is demonstrated here:
  [ecall (ECall p) = return p]
The part of the computation after the scope delimitation
is treated as a single opaque computation. Thus [call]
cannot prune away the internal structure.

If no internal structure can be observed, there is no way
to prune too much -}

exprGoodCut :: (Nondet ⊂ sig, Symbol ⊂ sig, Call ⊂ sig, Cut ⊂ sig) =>
               Prog sig Int
exprGoodCut = do { i <- term
                 ; call' ((do { symbol '+' ; cut ; j <- exprGoodCut ; return (i + j) })
                          `branching`
                          (do { return i })) }

-- Notice the handler for [Cut] is now outside of [parse]
goodParse = run . solutions . runCut . (parse "1") $ exprGoodCut

understand = run . solutions . runCut $ call' (branching (cut >> (return 1)) (return 2))

{--------------------------------------------------------
Exception
--------------------------------------------------------}

data Exc e cnt = Throw' e

instance Functor (Exc e) where
  fmap _ (Throw' e) = (Throw' e)

pattern Throw e <- (project -> Just (Throw' e))

throw :: (Exc e ⊂ sig) => e -> Prog sig a
throw e = inject (Throw' e)

runExc :: (Functor sig) =>
          Prog (Exc e + sig) a -> Prog sig (Either e a)
runExc (Return a) = return (Right a)
runExc (Throw e) = return (Left e)
runExc (Other op) = Op (fmap runExc op)

catch :: (Exc e ⊂ sig) =>
         Prog sig a ->
         (e -> Prog sig a) ->
         Prog sig a
catch (Return a) h = return a
catch (Throw e) h = h e
catch (Op op) h = Op (fmap (\p -> catch p h) op)

decr :: (State Int ⊂ sig, (Exc ()) ⊂ sig) => Prog sig ()
decr = do { x <- get
          ; if x > (0 :: Int) then put (pred x) else throw () }

tripleDecr :: (State Int ⊂ sig, (Exc ()) ⊂ sig) => Prog sig ()
tripleDecr = decr >> catch (decr >> decr) return

{- [catch] Gets Pushed in Too Early?

Consider [tripleDecr]:
[catch] is a scoped handler. In practice, it transform
[decr >> decr] into the following:

do x <- get
   if (x > 0) then do put (pred x)
                      do x <- get
                         if (x > 0) then do put (pred x)
                                    else return () -- <-- here we apply the [h]
              else return () -- <-- here we apply the [h]

It may be clear now why running [runState] after does not
let us reroll back to before the [catch]

Mainly because the information about scope is now lost -}

-- State end up 0
tooMuchStateOp :: Either () (Int, ())
tooMuchStateOp = (run
                  . runExc
                  . runState (2 :: Int) )
                 (tripleDecr :: Prog (State Int + (Exc () + Void)) ())

-- State end up still 0
-- This is a "meh" example.
-- After the handler [catch] has handled the inner computation
-- The handler [runExc] actually have nothing to do now
swapDoesNothing :: (Int, Either () ())
swapDoesNothing = (run
                   . runState (2 :: Int)
                   . runExc)
                  (tripleDecr :: Prog (Exc () + (State Int + Void)) ())

-- The first [decr] creates a [Throw] that is not recoverable
resultIsLeft :: Either () (Int, ())
resultIsLeft = (run
                 . runExc
                 . runState (0 :: Int) )
               (tripleDecr :: Prog (State Int + (Exc () + Void)) ())

-- The [Throw] happens inside of a [catch], and is recovered
resultIsRight :: Either () (Int, ())
resultIsRight = (run
                  . runExc
                  . runState (1 :: Int) )
                (tripleDecr :: Prog (State Int + (Exc () + Void)) ())

{- Ordering of Handlers

The paper mentions
> The [catch] embedded within [tripleDecr] cannot be
> exchanged with [runState], because it would change
> the scope created by [catch]

I think there are two ways to exchange the orders,
and I think both indeed lead to unintented meaning

1. Keep [catch] where it is, push [runState] inside [catch]
  This is incorrect, because it forces the two inner [decr]s
  to operate on a separate state. In reality, we want the
  three [decr]s to operate on the same state

2. Keep [runState] where it is, pull [catch] to outside
   of [runState]
  This is also incorrect. We want [tripleDecr] to have
  the following behaviour:
  - The first [decr] may error, in which case the error is
    not recoverable
  - The second and third [decr] may error, but are recoverable
    through [catch]
  If we pull [catch] out, then suddenly all [decr] are
  recoverable
-}

{- Example of Reordering Handlers

Recall we don't want the first [decr] be recoverable
So we should expect the following code to just give us [Left]
(Because the first [decr] will immediately try to decrement 0)

However, running it will give us [Right (-10, ())] -}

allRecoverable = (run
                   . runExc
                   . (\p -> (catch :: Prog (Exc () + Void) a
                                   -> (() -> Prog (Exc () + Void) a)
                                   -> Prog (Exc () + Void) a)
                            p (\_ -> return (-10, ())) )
                   . runState (0 :: Int) )
                 ((decr >> decr >> decr) :: Prog (State Int + (Exc () + Void)) ())

{--------------------------------------------------------
============================================================
============================================================
TODO
============================================================
============================================================

Section 9: Scoped Syntax Revisited
--------------------------------------------------------}
