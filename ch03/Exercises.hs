myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

mean xs = addAll xs / fromIntegral (myLength xs)
          where addAll []     = 0
                addAll (x:xs) = x + addAll xs

palindromise [] = []
palindromise a = a ++ rev a
  where rev (x:xs) = rev xs ++ [a]
        rev a      = a
