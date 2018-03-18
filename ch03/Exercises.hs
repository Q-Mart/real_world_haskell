import Data.List
import Trees

myLength :: [a] -> Int
myLength (_:xs) = 1 + myLength xs
myLength []   = 0

mean xs = sum xs / (fromIntegral $ length xs)

palindrome :: [a] -> [a]
palindrome xs = xs ++ reverse xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

sortLists :: Ord a => [[a]] -> [[a]]
sortLists xs = sortBy smallerList xs
               where smallerList as bs = compare (length as) (length bs)

myIntersperse :: a -> [[a]] -> [a]
myIntersperse _ [] =  []
myIntersperse _ [x] = x
myIntersperse c (x:xs) = x ++ [c] ++ myIntersperse c xs

height :: Tree a -> Int
height Empty = 0
height (Node _ left right) = 1 + (maximum $ [height left, height right])

data Direction = LeftTurn | RightTurn | StraightOn deriving (Show, Eq)
type Point  = (Float, Float)

direction :: Point -> Point -> Point -> Direction
direction  (x1, y1) (x2, y2) (x3, y3)
           | crossProduct > 0 = RightTurn
           | crossProduct == 0  = StraightOn
           | crossProduct < 0 = LeftTurn
           where crossProduct = (y2-y1)*(x3-x2) - (x2-x1)*(y3-y2)

directionOfTriples :: [Point] -> [Direction]
directionOfTriples ps
           | length ps < 3 = []
           | otherwise     = direction (ps!!0) (ps!!1) (ps!!2) : (directionOfTriples $ tail ps)

genPoint :: Float -> Point
genPoint x = (x, x+1)

comparePoints :: Point -> Point -> Ordering
comparePoints (x1, y1) (x2, y2)
           | y1 > y2              = GT
           | y1 == y2 && x1 > x2  = GT
           | y1 == y2 && x1 < x2  = LT
           | y1 == y2 && x1 == x2 = EQ
           | otherwise            = LT

bottomMost :: [Point] -> Point
bottomMost ps = head $ sortBy comparePoints ps

angleBetween :: Point -> Point -> Float
angleBetween (x1, y1) (x2, y2) = atan2 (y2 - y1) (x2 - x1)

sortPoints :: [Point] -> [Point]
sortPoints ps = (bottomMost ps):(sortBy angleBetweenLowest psWithoutBottomMost)
           where angleBetweenLowest a b = compare (angleBetween (bottomMost ps) (a)) (angleBetween (bottomMost ps) (b))
                 psWithoutBottomMost = delete (bottomMost ps) ps

-- From gmaths, idea is to discard right turns
grahamScan :: [Point] -> [Point]
grahamScan xs = gs (sortPoints xs)
           where gs (x:y:z:xs) = if direction x y z /= RightTurn
                                  then x:gs (y:z:xs)
                                  else gs (x:z:xs)
                 gs xs = xs

testData :: [Point]
testData =  [(-3,1),(-4,1),(-1,4),(0,0),(2,2),(-1,3),(-1,2),(1,0),(3,-1),(-1,-1)]

answer :: [Point]
answer = [(3.0,-1.0),(2.0,2.0),(-1.0,4.0),(-4.0,1.0),(-1.0,-1.0)]

testGrahamScan = sort (answer) == sort (grahamScan $ testData)
