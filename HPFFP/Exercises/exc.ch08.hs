data DividedResult = 
     Result Integer
   | DividedByZero deriving (Show)

{- function that recursively sums all numbers from 1 to n,
where n is the argument -}
sumrec :: (Eq a, Num a) => a -> a
sumrec 1 = 1
sumrec n = (sumrec (n-1)) + n

{- function that multiplies two integral numbers
using recursive summation -}
mu :: (Integral a) => a -> a -> a 
mu 0 _ = 0
mu _ 0 = 0
mu f 1 = f
mu f s = f + (mu f (s-1))

{- "div" function, made with recursion and special datatype -}
dividedBy :: Integer -> Integer -> DividedResult
dividedBy num denom = go num denom 0
    where go n   d count
           | d == 0    = DividedByZero
           | n < d     = Result count
           | otherwise = go (n - d) d (count + 1)

{- The McCarthy 91 function yields x - 10 when x > 100 and 91 otherwise.
The function is recursive-}
mc91 :: (Num a, Ord a) => a -> a
mc91 n
  | n > 100   = n - 10
  | otherwise = mc91 $ mc91 $ n + 11
