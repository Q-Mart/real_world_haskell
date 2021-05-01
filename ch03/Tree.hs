data Tree a = Node a (Tree a) (Tree a)
    | Empty
     deriving (Show)

data NewTree a = NewNode {
    node :: a,
    left :: Maybe (NewTree a),
    right :: Maybe (NewTree a)
} deriving (Show)

height Empty = 0
height (Node a b c) = 1 + max (height b) (height c)
    where
        max a b
            | a >= b = a
            | otherwise = b
