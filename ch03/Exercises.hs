import Data.List

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

mean xs = addAll xs / fromIntegral (myLength xs)
          where addAll []     = 0
                addAll (x:xs) = x + addAll xs

palindromise [] = []
palindromise a = a ++ rev a
  where rev (x:xs) = rev xs ++ [x]
        rev a      = a

isPalindrome a = rev a == a
  where rev (x:xs) = rev xs ++ [x]
        rev a      = a

-- on q6
sortLength xs = sortBy compareLengths xs
  where compareLengths a b = compare (length a) (length b)

myIntersperse _ [] = []
myIntersperse sep (x:xs)
  | xs == [] = x
  | otherwise = x ++ [sep] ++ myIntersperse sep xs
