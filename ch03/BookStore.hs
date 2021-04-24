data BookInfo = Book Int String [String]
  deriving (Show)

data MagazineInfo = Magazine Int String [String]
  deriving (Show)

data BookReview = BookReview BookInfo CustomerID String

type CustomerID = Int
type ReviewBody = String
type Address = [String]

type CardNumber = Int
type CardHolder = String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)

data BillingInfo = CreditCard CardNumber CardHolder Address
  | CashOnDeliver
  | Invoice CustomerID
   deriving (Show)

bookID (Book id title authors) = id
bookTitle (Book id title authors) = title
bookAuthors (Book id title authors) = authors

nicerID (Book id _ _) = id
nicerTitle (Book _ title _) = title
nicerAuthors (Book _ _ authors) = authors

data Customer = Customer {
  customerId :: CustomerID,
  customerName :: String,
  customerAddress :: Address
                         } deriving (Show)

myInfo = Book  9780135072455 "Algebra of Programming"
  ["Richard Bird", "Oege de Moor"]
