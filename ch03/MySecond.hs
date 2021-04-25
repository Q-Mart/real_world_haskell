mySecond :: [a] -> a

mySecond xs = if null (tail xs)
                 then error "list too short"
                 else head (tail xs)

safeSecond [] = Nothing
safeSecond xs = if null (tail xs)
                   then Nothing
                   else Just (head (tail xs))

tidySecond (_:x:_) = Just x
tidySecond _ = Nothing
