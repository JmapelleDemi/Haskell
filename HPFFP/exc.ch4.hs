module ExcerciseChapter4 where

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Woot = Blah
changeMood Blah = Woot

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = if x == reverse x then True else False

myAbs :: Integer -> Integer
myAbs x = if x < 0 then negate x else x 

f :: (a, b) -> (c, d) -> ((b, d), (a, c)) 
f x y = (,) ((,) (snd x) (snd y)) ((,) (fst x) (fst y))

ff xs = (length xs) + 1

ft :: (a,b) -> a
ft x = fst x