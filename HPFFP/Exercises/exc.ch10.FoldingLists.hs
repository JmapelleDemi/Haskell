import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime
                (fromGregorian 1991 5 1)
                (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
                (fromGregorian 1921 5 1)
                (secondsToDiffTime 34123))
    ]

{- Write a function that filters for DbDate values and returns  a list
of the UTCTime values inside them -}

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate []                = []
filterDbDate ((DbDate x) : xs) = x : filterDbDate xs
filterDbDate (_:xs)            =     filterDbDate xs

{- Write a function that filters for DbNumber values and returns a list
of the Integer values inside them -}

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber []                  = []
filterDbNumber ((DbNumber x) : xs) = x : filterDbNumber xs
filterDbNumber (_:xs)               =    filterDbNumber xs 

{- Write a function that gets the most recent date -}

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent list = maximum $ filterDbDate list

-- extra
mAnc :: [DatabaseItem] -> UTCTime
mAnc list       = minimum $ filterDbDate list

{- Write a function that sums all of the DbNumber values -}

sumDb :: [DatabaseItem] -> Integer
sumDb list = sum $ filterDbNumber list

{- Write a function that gets the average of the DbNumber values -}

avgDb list = (fromIntegral $    sum $ filterDbNumber list) /
             (fromIntegral $ length $ filterDbNumber list)

-- Scans exercises:

-- Modify your 'fibs'function to only return the first 20 Fibonacci numbers

fibs = take 20 $ 1 : scanl (+) 1 fibs

-- Modify 'fibs' to return the Fibonacci numbers that are less than 100

fibs2 = takeWhile (<100) $ 1 : scanl (+) 1 fibs

fibs2alt = filter (<100) $ 1 : scanl (+) 1 fibs -- alternative version

{- Try to write the 'factorial' function drom recursion as a scan.
You'll want scanl again, and your start value will be 1.
Warning:
this will also generate an infinite list, so you may want to pass it
through a 'take' function or similar -}

factGen :: [Int]
factGen = scanl (*) 1 [1..]

takeFact :: Int -> [Int]
takeFact n = take n factGen

-- Given the following sets of consonants and vowels:
stops  = "pbtdkg"
vowels = "aeiou"

{- Write a function that takes inputs from 'stops' and 'vowels' and
makes 3-tuples of all possible stop-vowel-stop combintaions. These will
not all correspond to real worlds in English, although the stop-vowel-stop
pattern is common enough that many of them will -}

svsGen :: [(Char,Char,Char)]
svsGen =  [(x,y,z) | x <- stops, y <- vowels, z <- stops]

-- Additional function that applied to svsGen to easy to read its result
showsvs :: [(Char,Char,Char)] -> [String]
showsvs []             = []
showsvs ((x,y,z):rest) = (x:[] ++ y:[] ++ z:[]) : showsvs rest 

{- Modify that function so that it only returns the combinations
that btgin with a 'p'-}

svsGenPFirst :: [(Char,Char,Char)]
svsGenPFirst =  [('p',y,z) | y <- vowels, z <- stops]

{- Now set up lists of nouns and verbs (instead of stops and vowels)
and modify the function to make tuples representing possible 
noun-verb-noun sentences -}

nouns = ["dog","cat","rat","bird","worm","fish"]
verbs = ["bark","sleep","sneak","sing","dig","swim"]

nvnGen :: [(String,String,String)]
nvnGen =  [(x,y,z) | x <- nouns, y <- verbs, z <- nouns]

-- Additional function that applied to nvnGen to easy to read its result
shownvn :: [(String,String,String)] -> [String]
shownvn []             = []
shownvn ((x,y,z):rest) = (x ++ y ++ z) : shownvn rest 

-- myOr returns True if any Bool in the list is True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

{- myAny returns True
if a -> Bool applied to any values in the list returns True -}

myAny :: (a -> Bool) -> [a] -> Bool
myAny f []     = False
myAny f (x:xs) = case f x of True  -> True
                             False -> myAny f xs

myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 f []     = False
myAny2 f (x:xs) = f x || myAny f xs

myAny3 :: (a -> Bool) -> [a] -> Bool
myAny3 f xs = foldr h False xs
  where h x y = f x || y

{- Write two versions of 'myElem'. One version should use folding
and the other should use 'any' -}

testElem :: Eq a => a -> [a] -> Bool
testElem el []     = False
testElem el [x]    =    el == x
testElem el (x:xs) = if el == x then True
                     else testElem el xs

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr f False
           where f a b = a == x || b

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 x = myAny3 (== x)

{- Iplement myReverse, don't worry to make it lazy -}

myReverse :: [a] -> [a]
myReverse = foldr rev []
            where rev x y = y ++ [x]

{- Write myMap in terms of foldr.
It should have the same behavior as the built-un 'map'-}

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ff []
          where ff x y = (f x) : y

{- Write myFilter in terms of foldr. It should have the same behavior
as the built-in 'filter' -}

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr ff []
             where ff x y = case f x of True  -> x : y
                                        False -> y

-- Squish flattens a lisy into a list

squish :: [[a]] -> [a]
squish = foldr sq []
         where sq x y = x ++ y

-- 'squishMap' maps a function over a list and concatenates the results

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr sq []
              where sq x y = (f x) ++ y

{- 'squishAgain' flattens a list of lists into a list.
This time re-use the 'squishMap' function -}

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

{- 'myMaximumBy' takes a comparison function and a list and returns
the greatest element of the list based on the last value that
the comparison returned GT for -}

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl ff x (x:xs)
                       where ff a b = if f a b == GT then a else b

{- 'myMinimumBy' takes a comparison function amd a list and returns
the least element of the list based on the last value that
the comparison returned LT for -}

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl ff x (x:xs)
                       where ff a b = if f a b == LT then a else b