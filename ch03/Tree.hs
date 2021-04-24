data Tree a = Node a (Tree a) (Tree a)
    | Empty
     deriving (Show)

data NewTree a = NewNode {
    node :: a,
    left :: Maybe (NewTree a),
    right :: Maybe (NewTree a)
} deriving (Show)

-- You are at reporting errors
