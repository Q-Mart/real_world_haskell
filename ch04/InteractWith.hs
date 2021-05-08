import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        -- myFunction = id
        myFunction = head . words

splitLines [] = []
splitLines cs =
  let (pre, suf) = break isLineTerminator cs
   in pre : case suf of
              ('\r':'\n':rest) -> splitLines rest
              ('\r':rest)      -> splitLines rest
              ('\n':rest)      -> splitLines rest
              _                -> []

isLineTerminator c = c =='\r' || c == '\n'

fixLines :: String -> String
fixLines input = unlines (splitLines input)

transpose s = unlines (transpose' [] (lines s))
  where transpose' os ls
          | all null ls = os
          | otherwise = transpose' (os ++ firstLetters ls) (map (drop 1) ls)
        firstLetters a = [map head a]
