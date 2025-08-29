module ImplicationIsHard where

class Foo a b where
  foo :: a -> b -> Int

instance Foo Int b
-- instance Foo a b => Foo [a] b

data Bar a where
   K :: a -> b -> Bar a

{-

Not really a type error,
there's an infinite set of possible types.

Main.hs:47:14: error: [GHC-39999]
    • No instance for ‘Foo a b’ arising from a use of ‘foo’
      Possible fix:
        add (Foo a b) to the context of the data constructor ‘K’
    • In the expression: foo x y
      In an equation for ‘f3’: f3 (K x y) = foo x y
-}
f3 :: Bar Int -> Int
-- f3 :: Bar [Int] -> Int
f3 (K x y) = foo x y
