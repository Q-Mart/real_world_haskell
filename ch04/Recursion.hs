import Data.Char (digitToInt)
import Data.Char (ord)
import Data.Bits (shiftL, (.&.), (.|.))

asInt :: String -> Int
asInt xs = loop 0 xs

loop :: Int -> String -> Int
loop acc [] = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
          in loop acc' xs

square :: [Double] -> [Double]
square (x:xs) = x*x : square xs
square [] = []

square2 xs = map squareOne xs
  where squareOne x = x * x

myMap :: (a -> b) -> [a] -> [b]

myMap f (x:xs) = f x : myMap f xs
myMap _ _ = []

oddList :: [Int] -> [Int]

oddList (x:xs)
  | odd x = x : oddList xs
  | otherwise = oddList xs
oddList _ = []

mySum xs = helper 0 xs
  where helper acc (x:xs) = helper (acc + x) xs
        helpec acc _ = acc

base = 65521

adler32 xs = helper 1 0 xs
  where helper a b (x:xs) = let a' = (a + (ord x .&. 0xff)) `mod` base
                                b' = (a' + b) `mod` base
                            in helper a' b' xs
        helper a b _ = (b `shiftL` 16) .|. a

adler32_try2 xs = helper (1,0) xs
  where helper (a,b) (x:xs) =
          let a' = (a + (ord x .&. 0xff)) `mod` base
              b' = (a' + b) `mod` base
          in helper (a',b') xs
        helper (a,b)_ = (b `shiftL` 16) .|. a

foldlSum xs = foldl step 0 xs
  where step acc x = acc + x

niceSum :: [Integer] -> Integer
niceSum xs = foldl (+) 0 xs

adler_32_foldl xs = let (a,b) = foldl step (1,0) xs
    in (b `shiftL` 16) .|. a
  where step (a,b) x = let a' = a + (ord x .&. 0xff)
              in (a' `mod` base, (a' + b) `mod` base)

myFoldl::(a->b->a) -> a -> [b] -> a
myFoldl f z xs = foldr step id xs z
  where step x g a = g (f a x)

append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs
