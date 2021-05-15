import Data.Char (digitToInt)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs = case break f xs  of
                   ([],xs) -> [xs]
                   (xs,[]) -> [xs]
                   (first, x:rest) -> [first] ++ splitWith f rest

asInt_fold :: String -> Int
asInt_fold xs
  | head xs == '-' = (-1) * (digitsToInts $ tail xs)
  | otherwise = digitsToInts xs where
      digitsToInts xs' = foldl addDigit 0 xs'
      addDigit acc x = acc * 10 + digitToInt x

-- On exercise 2
