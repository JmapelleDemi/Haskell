-- Write Vigenere cipher

module VigenereCipher where

import Data.Char

decode :: String -> String -> String
decode []   key = []
decode text key = helper text key key
                  where
                  helper []     (y:ys) key = []
                  helper (x:xs) []     key = helper (x:xs) key key
                  helper (x:xs) (y:ys) key = chr (ord x + ord y) : helper xs ys key

encode :: String -> String -> String
encode []   key = []
encode text key = helper text key key
                  where
                  helper []     (y:ys) key = []
                  helper (x:xs) []     key = helper (x:xs) key key
                  helper (x:xs) (y:ys) key = chr (ord x - ord y) : helper xs ys key

{- This should return True of (and only if) all the values in the firstlist appear 
in the second list, thought they need to be contiguous -}

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf []     text         = True
isSubsequenceOf _      []           = False
isSubsequenceOf what@(w:ws) (x:xs)
  | w == x    = isSubsequenceOf ws xs
  | otherwise = isSubsequenceOf what xs 

-- Split a sentence into words, then tuple each word with the capitalized form of each

capitalizeWords :: String -> [(String, String)]
capitalizeWords []          = []
capitalizeWords text@(x:xs) = toTuple fw : capitalizeWords rest
                              where
                              fw = head . words $ text 
                              toTuple x = (x, toUpper (head x) : (tail x))
                              rest = unwords . tail . words $ text

-- Write a function that capitalizes a word

capitalizeWord :: String -> String
capitalizeWord []     = []
capitalizeWord (x:xs) = toUpper x : xs

{- Write a function that capitalizes sentences in a paragraph. Recognize when
a new sentence has begun by checking for periods. Reuse the 'capitalizeWords' function -}

capitalizeParagraph :: String -> String
capitalizeParagraph []          = []
capitalizeParagraph text = check (capitalizeWord text)
                           where
                           check [] = []
                           check ('.':' ':rest) = ". " ++ check (capitalizeWord rest)
                           check (x:xs)         = x : check xs