fmapWrapper :: (Functor myfun) => (a -> b) -> myfun a -> myfun b
fmapWrapper f ca = fmap f ca

main :: IO ()
main = return ()
