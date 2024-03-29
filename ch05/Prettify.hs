import SimpleJSON
import Numeric

data Doc = ToBeDefined deriving (Show)

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
              Just r -> text r
              Nothing
                | mustEscape c -> hexEscape c
                | otherwise -> char c
  where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where ch a b = (a, ['\\',b])

smallHex :: Int -> Doc
smallHex x = text "\\u"
    <> text (replicate (4 - length h) '0')
    <> text h
  where h = showHex x ""

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined
