module Cipher where

import Data.Char

{- Write a basic Caesar cipher that shifts rightward -}

decode :: Int -> String -> String
decode shift []     = []
decode shift (x:xs) = case x == ' ' of True  -> x : decode shift xs
                                       False -> chr (ord x + shift) : decode shift xs

encode :: Int -> String -> String
encode shift []     = []
encode shift (x:xs) = case x == ' ' of True  -> x : encode shift xs
                                       False -> chr (ord x - shift) : encode shift xs