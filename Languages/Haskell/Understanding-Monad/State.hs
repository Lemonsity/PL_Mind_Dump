import Control.Monad

{- On the Naming

In some sense, we should not call the monad "State"
(State s a) is a really an "action", stored under
the key [runState]
The [action] makes sense in an environment where
there is a state of type [s] surrounding it -}

newtype State s a = State { runState :: s -> (a, s) }

-- This creates a new action that can be performed
-- on a state of type [s]
state :: (s -> (a, s)) -> State s a
state = State

{- What if I want parametrized action?

I may consider a function of the following type:
    generateAction :: param -> State s a
-}

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Monad (State s) where
  return :: a -> State s a
  return x = state (x,)

  (>>=) :: State s a -> (a -> State s b) -> State s b
  p >>= f = state $ \ sInit ->
    let (val, sMid) = runState p sInit
    in runState (f val) sMid 

{- "I only want the return value / new state! " -}
-- Only return the value
evalState :: State s a -> s -> a
evalState p sInit = fst (runState p sInit)
-- Only return the new state
execState :: State s a -> s -> s
execState p sInit = snd (runState p sInit)


{- Setting State in Testing -}
put :: s -> State s ()
put newState = state $ const ((), newState)

{- Getting the State in the middle of binding -}
get :: State s s
get = state $ \s -> (s, s)

{- Example with Turnstile -}
data TurnstileState = Locked | Unlocked deriving (Eq, Show)
data TurnstileOutput = Thanks | Tut | Open deriving (Eq, Show)
data TurnstileInput = Coin | Push deriving (Eq, Show)

push :: State TurnstileState TurnstileOutput
push  = do
  s <- get -- The [s] here is the [fst] of [(s, s)] returned by [\s -> (s,s)]
  put Locked
  case s of
    Locked -> return Tut
    Unlocked -> return Open

-- This is the parametrized action that we considered near the beginning
turnS :: TurnstileInput -> State TurnstileState TurnstileOutput
turnS = state . turn where
  turn Coin = \_ -> (Thanks, Unlocked)
  turn Push = \s -> if s == Unlocked
                    then (Open, Locked)
                    else (Tut, Locked)
-- ^^ I explicitly constructed the function [state] is expecting
-- Can be optimized to following vv
turnS' :: TurnstileInput -> State TurnstileState TurnstileOutput
turnS' = state . turn where
  turn Coin _ = (Thanks, Unlocked)
  turn Push Unlocked = (Open, Locked)
  turn Push Locked = (Tut, Locked)

-- Tracking Output and Final State
stateTransition :: ([TurnstileOutput], TurnstileState)
stateTransition =
  runState
  -- Recall following translation
  --   mapM f list
  -- = traverse f list
  -- = foldr cons_f (pure []) list
  --       where cons_f x ys = liftA2 (:) (f x) ys
  --
  -- Pay attention to [f x], in our case, we have:
  --     + [x :: TurnstileInput]
  --     + [f x :: State TurnstileState TurnstileOutput]
  -- By type signature of [liftA2], we have
  --     + [ys :: State TurnstileState [TurnstileOutput]
  --
  -- We replace [f] with the actual [turnS], then
  -- [liftA2 (:) (turnS x) ys] actually translate to
  -- = ap (fmap (:) (turnS x)) ys
  -- = ap (liftM (:) (turnS x)) ys
  -- = ap (do { output1 <- (turnS x); return ((:) output1) }) ys
  -- = do {
  --     cons_output1 <- (do { output1 <- (turnS x); return ((:) output1) });
  --     outputs <- ys;
  --     return (cons_output1 outputs) }
  --
  -- The rough idea of the program is as follows
  --     + Start with an init state
  --     + Take the head of the list of inputs
  --     + Map the input to an action
  --     + With the init state and action, produce an output [output1]
  --        + In the background, we also get a new state
  --     + [return ((:) output1)] keeps the state, but turn [output1]
  --       into a function that concat [output1] to the front of some list
  --     + [ys] starts with the new state, generates a list of output, and
  --       the final state
  --     + [return (cons_output1 outputs)] prepend [output1], the output
  --       of the first action to the list of output, and keep the final state
  (mapM turnS [Coin, Push, Push, Coin, Push])
  Locked




