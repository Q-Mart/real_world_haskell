main = interact wordCount
  -- counts the number of lines
  -- where wordCount input = show (length (lines input)) ++ "\n"
  -- counts the number of words
  -- where wordCount input = show (length (words input)) ++ "\n"
  -- counts the number of chars
  where wordCount input = show (length (input)) ++ "\n"
