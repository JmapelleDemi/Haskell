k (x, y) = x
k1 = k ((4-1),10)
k2 = k ("three", (1+2))
k3 = k (3, True)

f :: (a, b, c) -> (d, e, f) -> ((a,d), (c,f))
f (a,b,c) (d,e,f) = ((a,d),(c,f))

funcC x y = 
   case x > y of
      True  -> x
      False -> y

ifEvenAdd2 n =
   case even n of
      True  -> n + 2
      False -> n

nums x =
   case compare x 0 of
      LT -> -1
      GT -> 1
      EQ -> 0

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where y = x / 100

tensDigit :: Integral a => a -> a
tensDigit = fst . d10 . snd . d100 
            where d10  x = x `divMod` 10
                  d100 x = x `divMod` 100

hunsD :: Integral a => a -> a
hunsD = fst . d100 . snd . d1000
        where d100 x   = x `divMod` 100
              d1000 x = x `divMod` 1000

foldBool :: a -> a -> Bool -> a
foldBool x y bool =
    case bool of True  -> x
                 False -> y

fb2 :: a -> a -> Bool -> a
fb2 x y bool
 | bool == True = x
 | otherwise    = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a,c) = (f a,c)