myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

mean xs = sum xs / fromIntegral (length xs)

palindromise [] = []
palindromise (x:xs) = xs ++ palindromise xs
