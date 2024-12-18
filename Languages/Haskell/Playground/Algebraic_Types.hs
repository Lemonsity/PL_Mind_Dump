{-# LANGUAGE GADTs #-}

data MyData a where
  C1 :: MyData a
  C2 :: (MyData b) -> (MyData c) -> MyData (Either c a)

something = C1
