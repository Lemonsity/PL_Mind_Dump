{-# LANGUAGE GADTs, NoMonoLocalBinds #-}  -- NoMonoLocalBinds to enable let gen

data T a where
  C1 :: Bool -> T Bool
  C2 :: T a

f = \w ->         -- Give w :: alpha
      \e ->       --
        case e of                  -- Give e :: T alpha, the return type of branchs beta
          C1 b -> let temp = not w -- Give temp :: rho, have (gamma -> rho) ~ (Bool -> Bool)
                        -- "Supposedly solved" to get { rho := Bool, gamma := Bool }
                  in temp -- ok, w is unified with Bool
          C2 -> False

{-
g = \w ->               -- Give w :: gamma
      \e ->             --
        case e of       -- Give e :: T alpha, the return type of branchs beta
          C1 b -> not w -- not ok, w cannot be unified with Bool
          C2 -> False
-}

data R where
  MkR :: (b1 ~ b2) => b1 -> b2 -> R

forceType :: Int -> Int
forceType x = x

foo t = let f = \x -> case t of
                      MkR b1 b2 -> forceType x
        in f

bar t = let f = \x -> case t of
                      MkR b1 b2 -> x + 1
        in f


{---------------------------------------------------
Let binds a lower level skolem
---------------------------------------------------}
-- data S a where
--   MkS :: forall b . b -> S Bool

-- baz = \s -> case s of
--               MkS b -> let temp = \x -> b x
--                        in ()

data K a where
  MkK1 :: (a ~ Int) => a -> K a
  MkK2 :: K a

myfun x y =
  case x of
    MkK1 _ -> case y of
                MkK1 _ -> True
                MkK2 -> True
    MkK2 -> True
