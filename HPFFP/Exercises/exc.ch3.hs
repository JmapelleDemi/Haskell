module ExcerciseChapter3 where

funcA :: String -> String
funcA str = str ++ "!" 

funcB :: String -> String
funcB str = take 1 $ drop 4 str

funcC :: String -> String
funcC str = head $ reverse $ words str

thirdLetter :: String -> Char
thirdLetter str = head $ drop 2 str 

thirdLetter' :: String -> Char
thirdLetter' str = str !! 2

letterIndex :: String -> Int -> Char
letterIndex str n = str !! (n - 1)

rvrs :: String -> String
rvrs str = let z = words str
           in  (z !! 2) ++ " " ++ (z !! 1) ++ " " ++ (z !! 0)
