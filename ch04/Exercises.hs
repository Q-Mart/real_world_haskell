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

asInt_fold_error :: String -> Int
asInt_fold_error xs
  | xs == "" = error "Cannot call asInt_fold with empty string"
  | xs == "-" = error "Cannot call asInt_fold with string \"-\""
  | any (== '.') xs = error "Does not work with floating points"
  | otherwise = asInt_fold xs

type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int
asInt_either xs
  | xs == "" = Left "Empty string"
  | xs == "-" = Left "Non number '-'"
  | any (== '.') xs = Left $ "Floating point " ++ xs
  | otherwise = Right $ asInt_fold xs

myConcat :: [[a]] -> [a]
myConcat xs = foldr (++) [] xs

takeWhileExplicit _ [] = []
takeWhileExplicit f (x:xs)
  | f x == True = [x] ++ takeWhileExplicit f xs
  | otherwise = []

takeWhileFold f xs = foldr appendIfTrue [] xs where
  appendIfTrue x acc =  case f x of
                         True -> x:acc
                         False -> []

-- On exercise 5
