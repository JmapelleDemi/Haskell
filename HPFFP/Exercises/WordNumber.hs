module WordNumber where

import Data.List (intersperse)

{- First write a function that turns integers from 0-9 into their
corresponding English words, "one", "two", and so on. The you will
write a function that takes the integer, separates the digits, and
returns it as a list of integers. Finally you will need to apply 
the first function to the list produced by the second function and
turn it into a single string with interspersed hyphens -}

digitToWord :: Int -> String
digitToWord n = case n of 0 -> "zero"
                          1 -> "one"
                          2 -> "two"
                          3 -> "three"
                          4 -> "four"
                          5 -> "five"
                          6 -> "six"
                          7 -> "seven"
                          8 -> "eight"
                          9 -> "nine"

digits :: Int -> [Int]
digits n 
  | n > 9     = digits (div n 10) ++ [mod n 10]
  | otherwise =  [n]

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" (map digitToWord (digits n))

{- Fuction that produce same result as wordNumber function, but
does not use digits function -}

extra :: Int -> String
extra n 
  | n > 9     = extra (div n 10) ++ "-" ++ ( digitToWord ( mod n 10) )
  | otherwise = digitToWord n