import Data.Bool

{- Write your own enumFromTo definitions for the types provided. Do
not use range syntax to do so. It should result as if you didi 
[start..stop] -}

eftBool :: Bool -> Bool -> [Bool]
eftBool False True  = [False,True]
eftBool False _     = [False]
eftBool True  True  = [True]
eftBool True  _     = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd GT GT = [GT]
eftOrd fo fs = if fo <= fs then [fo] ++ (eftOrd (succ fo) fs) else []

eftInt :: Int -> Int -> [Int]
eftInt fi si 
 | fi >  si = []
 | fi <= si = [fi] ++ (eftInt (succ fi) si)

eftChar :: Char -> Char -> [Char]
eftChar fc sc
 | fc >  sc = []
 | fc <= sc = [fc] ++ (eftChar (succ fc) sc) 

{- Using takeWhile and dropWhile, write a function that takes a string
and returns a list of strings, using spaces to separate the elements
of the string into words, as in the following example:

"have some fun" - > ["have", "some", "fun"] -}

myWords :: String -> [String]
myWords []  = []
myWords str = [takeWhile (/= ' ') str] ++
              myWords (dropWhile (== ' ') . dropWhile (/= ' ') $  str)

{- Write a function that takes a string and returns a list of strings, using
newline separator to break up the string as in the following.
page 308  -}

myLines :: String -> [String]
myLines []  = []
myLines str = [takeWhile (/= '\n') str] ++ 
              myLines (deln str)
              where  
              deln = dropWhile (== '\n') . dropWhile (/= '\n')

{- Try writing a new function that parameterizes the character you're
breaking the string argument on and rewrite myWords and myLines using it -}

breakingLines :: Char -> String -> [String]
breakingLines ch []  = []
breakingLines ch str = [takeWhile (/= ch) str] ++ 
                       breakingLines ch (delch str)
                       where  
                       delch = dropWhile (== ch) . dropWhile (/= ch)

{- Given the following:-}
mySqr  = [x^2 | x <- [1..5]]
myCube = [x^3 | x <- [1..5]]
{-Write an expression that will make tuples
of the outputs of mySqr and myCube-}

tupe = [(x,y) | x <- mySqr, y <- myCube ]

{-Alter that expression so that in only
uses x and y values that are less than 50-}

tupe' = [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50 ]

{-Apply another function to that list comprehension to determine
how many tuples inhabit your output list-}

tupeCounter :: [(a,b)] -> Int
tupeCounter = length -- :)    -> 15 tuples

{- Write function that does the same as the map (if-then-else) function
you saw above  ->    map (\x -> if x == 3 then (-x) else ()) [1..10] 
but uses bool instead of the if-then-else syntax. Your first
step should be bringing the bool function into scope by typing
import Data.Bool at you Prelude prompt -}

foldBool :: (Num b, Eq b) => [b] -> [b]
foldBool = map (\x -> bool (-x) x (x == 3))

{- How might we write a filter function that would give us all the 
multiples of 3 out of a list from 1-30 -}

newF = filter (\x -> rem x 3 == 0) [1..30]

{- How could we compose the above function with the length function
to tell us *how many* multiplies of 3 there are between 1 and 30? -}

newF2 = length . filter (\x -> x /= 30 && x /= 1) $ newF

{- We're going to work on removing all articles ('the', 'a', 'an')
from sentences -}

remArt :: String -> String
remArt []  = []
remArt str =  unwords . check . words $ str
              where check []     = []
                    check (x:xs) = case x == "a"
                                     || x == "the"   
                                     || x == "an" of True  -> check xs
                                                     False -> x : check xs

{- Write your own version of zip :: [a] -> [b] -> [(a,b)] and
ensure it behaves the same as the original -}

myzip :: [a] -> [b] -> [(a,b)]
myzip [] []         = []
myzip _ []          = []
myzip [] _          = []
myzip (x:xs) (y:ys) = [(x,y)] ++ myzip xs ys

{- Do what you did for zip, but now for
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c] -}

myZW :: (a -> b -> c) -> [a] -> [b] -> [c]
myZW f [] _         = []
myZW f _ []         = []
myZW f (x:xs) (y:ys)= [f x y] ++ myZW f xs ys

{- Rewrite your zip in terms of the zipWith you wrote -}

myzip2 :: [a] -> [b] -> [(a,b)]
myzip2 [] []         = []
myzip2 _ []          = []
myzip2 [] _          = []
myzip2 fstr sstr = myZW (,) fstr sstr
