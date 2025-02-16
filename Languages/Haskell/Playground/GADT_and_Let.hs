{- Credit to Andong Fan -}

data T a where
  C :: Bool -> T Bool

f :: Int -> T a -> (Int, Int)
f w e = case e of
          C b -> let y g = (g b, w + 1) in y (\_ -> 1)
                  
-- f' :: Integer -> T a -> Integer
f' w e = case e of
           C b -> let y _ = w + 1
                  in 1

main :: IO ()
main = return ()                     
