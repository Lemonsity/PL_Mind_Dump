{-# LANGUAGE GADTs, NoMonoLocalBinds #-}

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

g = \w ->               -- Give w :: gamma
      \e ->             -- 
        case e of       -- Give e :: T alpha, the return type of branchs beta
          C1 b -> not w -- not ok, w cannot be unified with Bool
          C2 -> False

