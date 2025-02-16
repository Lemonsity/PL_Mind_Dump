{-# LANGUAGE GADTs, NoMonoLocalBinds #-}

data T a where
  C1 :: Bool -> T Bool
  C2 :: T a


{-
MonoLocalBinds:   Fail to infer
NoMonoLocalBinds: Infer Bool -> T a -> Bool
-}
-- Fail to infer w/ MonoLocalBinds
-- Infer Bool -> T a -> Bool w/ NoMonoLocalBinds
f w e = case e of
          C1 b -> let y _ = not w in True
          C2   -> False
