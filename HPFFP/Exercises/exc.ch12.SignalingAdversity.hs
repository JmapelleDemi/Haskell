{- Write a recursive function named 'replaceThe' which takes a text/string,
breaks it into words and replaces each instance of "the" with "a".
It's intended only to replace exactly the word "the".
'notThe' is suggested helper function dor accomplishing this -}
import Data.Char

data BinaryTree a = Leaf
                  | Node (BinaryTree a) a (BinaryTree a)
                  deriving (Eq, Ord, Show)

replaceThe :: String -> String
replaceThe []   = []
replaceThe text = unwords.map replaceHelper.map notThe.words.map toLower $ text

notThe :: String -> Maybe String
notThe []    = Just []
notThe "the" = Nothing
notThe x     = Just x 

replaceHelper :: Maybe String -> String
replaceHelper (Just x) = x
replaceHelper Nothing  = "a"

{- Write a recursive finction that takes a text/string, breaks it into words,
and count the number of "the" followed by a vowel-initial word -}

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel []   = 0
countTheBeforeVowel text = countHelper.words $ text

countHelper :: [String] -> Integer
countHelper []           = 0
countHelper [[]]         = 0
countHelper ("The":rest) = if elem (head (head rest)) "aeiou" == True then 1 + countHelper rest else countHelper rest
countHelper ("the":rest) = if elem (head (head rest)) "aeiou" == True then 1 + countHelper rest else countHelper rest
countHelper (f:rest)     = countHelper rest

-- Return the number of letters that are vowels in the word

countVowels :: String -> Integer
countVowels []     = 0
countVowels (x:xs) = case elem x "aeiou" of True  -> 1 + countVowels xs
                                            False -> countVowels xs

{- Use the 'Maybe' type to write a function that counts the number of vowels
in a string and the number of consonants. If the number of vowels exceeds 
the number of consonants, the function returns 'Nothing'. In many human languages,
vowels are rarely exceeds the number of consonants so when they do, it indicates
the input isn't a real word (that is, a valid input to your dataset) -}

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord str = check (countVowels str) (countCons str)
             where
             check x y = case x < y of True  -> Just $ Word' $ show x
                                       False -> Nothing

countCons :: String -> Integer
countCons []  = 0
countCons str = fromIntegral (length str) - (countVowels str)

{- You'll be preseted with a datatype to represent the natural numbers.
The only values representable with the naturals are whole numbers from zero
to infinity. Your task will be implement functions to convert 'Natural's to 'Integer's
and 'Integer's to 'Natural's. The conversion from 'Natural's to 'Integer's won't return
'Maybe' because 'Integer' is a strict superset of 'Natural'. Any 'Natural'
can be represented by an 'Integer', but the same is not true of any 'Integer'.
Negative numbers are not valid numbers -}

data Nat = Zero | Succ Nat deriving (Eq, Show)

{-
>>natToInteger Zero
0
>>natToInteger (Succ Zero)
1
>>natToInteger (Succ (Succ Zero))
2                                               -}

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat x = if x < 0 then Nothing
                          else Just $ f x
                               where
                               f 0 = Zero
                               f x = Succ $ f (x - 1)

-- Write simple boolean checks for 'Maybe' values

isJust :: Maybe a -> Bool
isJust (Just x) = True
isJust _        = False 

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

{- The following is the 'Maybe' catamorphism. You can turn a 'Maybe' value
into anything else with this

>>mayybee 0 (+1) Nothing
0
>>mayybee 0 (+1) (Just 1)
2                                    -}

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x f Nothing  = x
mayybee x f (Just y) = f y

-- In case you just want to provide a fallback value

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing  = x
fromMaybe x (Just y) = y

{- Converting between 'List' and 'Maybe'

>>listToMaybe [1,2,3]
Just 1
>>listToMaybe []
Nothing                              -}

listToMaybe :: [a] -> Maybe a
listToMaybe []     = Nothing
listToMaybe (x:xs) = Just x

{-
>>maybeToList (Just 1)
[1]
>>maybeToList Nothing
[]                                   -}

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

{- For when we just want to drop the Nothing values from our list

>>catMaybes [Just 1, Nothing, Just 2]
[1, 2]
> catMaybes [Nothing, Nothing, Nothing]
[]                                   -}

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just x:rest)  = x : catMaybes rest
catMaybes (Nothing:rest) = catMaybes rest

{- You’ll see this called “sequence” later.

>> flipMaybe [Just 1, Just 2, Just 3]
Just [1, 2, 3]
>> flipMaybe [Just 1, Nothing, Just 3]
Nothing                              -}

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe []   = Nothing
flipMaybe list = if (length list) /= (length (catMaybes list))
                 then Nothing
                 else Just $ catMaybes list

{- Try to eventually arrive at a solution that uses 'foldr', even if
earlier versions don’t use foldr -}

lefts' :: [Either a b] -> [a]
lefts' list = foldr f [] list
              where
              f (Left x) y = x : y
              f x y        = y

-- Same as the last one. Use foldr eventually

rights' :: [Either a b] -> [b]
rights' list = foldr f [] list
               where
               f (Right x) y = x : y
               f x y         = y

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' list = (lefts' list, rights' list)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x) = Just $ f x
eitherMaybe' f _         = Nothing

-- This is a general catamorphism for Either values.

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f1 f2 (Right x) = f2 x
either' f1 f2 (Left  x) = f1 x

-- Same as before, but use the either' function you just wrote.

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f x = either' (const Nothing) (Just .f) x

{- Write the function myIterate using direct recursion. Compare
the behavior with the built-in iterate to gauge correctness. Do
not look at the source or any examples of iterate so that you
are forced to do this yourself -}

myIterate :: (a -> a) -> a -> [a]
myIterate f x = [f x] ++ myIterate f (f x)

{- Write the function myUnfoldr using direct recursion. Compare
with the built-in unfoldr to check your implementation. Again,
don’t look at implementations of unfoldr so that you ﬁgure it
out yourself -}

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = case f x of Nothing    -> []
                            Just (a,b) -> a : myUnfoldr f b

{- Rewrite myIterate into betterIterate using myUnfoldr. A hint —
we used unfoldr to produce the same results as iterate earlier.
Do this with diﬀerent functions and see if you can abstract the
structure out -}

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\a -> Just (a, f a)) x 

-- Write unfold for BinaryTree

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = case f x of Nothing        -> Leaf
                         (Just (a,b,c)) -> Node (unfold f a) b (unfold f c)

{- Make a tree builder.
Using the unfold function you’ve just made for BinaryTree, write
the following function  -}

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = takeTree n $ unfold f 0
              where
              f x = Just (x+1,x,x+1)
              takeTree 0 tree         = Leaf
              takeTree n Leaf         = Leaf
              takeTree n (Node a b c) = Node (takeTree (n-1) a) b (takeTree (n-1) c)
