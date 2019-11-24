import Data.Char

{- Write that function such that, given the input "HbEbLnLcO",
your function will return "HELLO" -}

onlyUp :: String -> String
onlyUp []     = []
onlyUp (x:xs) = case isUpper x of True  -> x : onlyUp xs
                                  False ->     onlyUp xs

{- Write a function that will capitalize the first letter of a String
and return the entire String. For example, if given the argumnet "julie", 
it will return "Julie" -}

capitalize :: String -> String
capitalize []     = []
capitalize (x:[]) = toUpper x : []
capitalize (x:xs) = toUpper x : xs

{- Now make a new version of that function that is recursive such that
if you give it the input "woot" it will holler back at yoo "WOOT" -}

capAll :: String -> String
capAll []     = []
capAll (x:xs) = toUpper x : capAll xs

{- Write a function that will capitalize the first letter of a String 
and return only that letter of a String and return only that letter
as the result -}

capHead :: String -> Char
capHead xs = toUpper . head $ xs

{- myOr returns True if any Bool in the list is True -}

myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = case x of True  -> True
                        False -> myOr xs

{- myAny returns True if 'a -> Bool' applied to any of the values in the list
returns True -}

myAny :: (a -> Bool) -> [a] -> Bool
myAny f []     = False 
myAny f (x:xs) = case f x of True  -> True
                             False -> myAny f xs

{- After you write the recursive myElem,
write another version that uses 'any'.
'elem' uses Foldable type, write version only for lists -}

myElem :: Eq a => a -> [a] -> Bool
myElem el []     = False 
myElem el (x:xs) = case x == el of True  -> True
                                   False -> myElem el xs

myEl2 :: Eq a => a -> [a] -> Bool
myEl2 el list = myAny (== el) list

{- Implement myReverse -}

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

{- squish flattens a list of lists into a list -}

squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

{- squishMap maps a function over a list and concatenates the results -}

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f []   = []
squishMap f list = squish $ map f list

{- squishAgain flattens a list of lists into a list.
This time re-use squishMap function -}

squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain list = squishMap id list

{- myMaximumBy takes a comparison function and a list and returns the greatest
element of the list based on the last value that the comparison returned GT for.
If you import 'maximumBy' from Data.List, you'll see the type is
Foldable t => (a -> a -> Ordering) -> t a -> a       rather than 
(a -> a -> Ordering) -> [a] -> a -}

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f [x]      = x
myMaximumBy f (x:y:[]) = if (f x y) == GT then x else y
myMaximumBy f (x:y:xs) = case f x y of LT -> myMaximumBy f (y:xs)
                                       EQ -> myMaximumBy f (y:xs)
                                       GT -> myMaximumBy f (x:xs)

{- myMinimumBy takes a comparison function and a list and returns the least 
element of the list based on the last value that the comparison 
returned LT for -}

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f [x]      = x
myMinimumBy f (x:y:[]) = if (f x y) == LT then x else y
myMinimumBy f (x:y:xs) = case f x y of LT -> myMinimumBy f (x:xs)
                                       EQ -> myMinimumBy f (x:xs)
                                       GT -> myMinimumBy f (y:xs)

{- Using the myMinimumBy and myMaximumBy functions, write your own versions
of maximum and minimum -}

myMaximum :: (Ord a) => [a] -> a
myMaximum str = myMaximumBy compare str

myMinimum :: (Ord a) => [a] -> a
myMinimum str = myMinimumBy compare str