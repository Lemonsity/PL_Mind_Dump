data Bit = Zero | One

bitToInt :: Bit -> Int
bitToInt Zero = 0
bitToInt One = 1

bigEndian, smallEndian :: [Bit] -> Int
bigEndian   digits = foldl (\acc digit -> 2 * acc + bitToInt digit) 0 digits
smallEndian digits = _

meanSquaredError :: [Int] -> [Int] -> Int
meanSquaredError xs ys =
  sum $
  zipWith (\x y -> (x-y) ^ 2) xs ys

