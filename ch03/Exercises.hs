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

sortLength xs = sortBy compareLengths xs
  where compareLengths a b = compare (length a) (length b)

myIntersperse _ [] = []
myIntersperse sep (x:xs)
  | xs == [] = x
  | otherwise = x ++ [sep] ++ myIntersperse sep xs

data Direction = Straight | Left | Right
     deriving (Eq, Show)

data Cartesian2D = Cartesian2D Double Double
  deriving (Eq, Show)

crossProduct (x1, y1) (x2, y2) (x3, y3) = p1 - p2
  where
    p1 = (x2 - x1) * (y3 - y1)
    p2 = (y2 - y1) * (x3 - x1)

dir p1 p2 p3
  | cp == 0 = Main.Straight
  | cp > 0 = Main.Left
  | otherwise = Main.Right
  where
    cp = crossProduct p1 p2 p3

dirs [a,b,c] = [dir a b c]
dirs xs = dirs (take 3 xs) ++ dirs (tail xs)

bottomeLeft ps = head $ sortBy (\(_,y1) (_,y2) -> compare y1 y2) ps

sortByAngle [] = []
sortByAngle ps = p0 : sortOn angle rest
  where
    p0@(x0,y0) = bottomeLeft ps
    rest = filter (/= p0) ps
    angle (x,y) = atan2 (y-y0) (x-x0)

grahamScan [] = []
grahamScan ps = scan [p0] sortedPs
  where
    p0:sortedPs = sortByAngle ps
    scan xs [] = xs
    scan xs [y] = y:xs
    scan (x:xs) (y:z:ys)
      | dir x y z == Main.Left = scan (y:x:xs) (z:ys)
      | otherwise = scan xs (x:z:ys)
