bigNum = (^) 5 10
--wahoo = bigNum $ 10
x = print
y = print "woohoo!"
z = x "hello world!"

funcH :: [a] -> a
funcH (x:_) = x

funcC :: Ord a => a -> a -> Bool
funcC x y = if (x > y) then True else False

funcS :: (a,b) -> b
funcS (x,y) = y 

i :: a -> a
i x = x

c :: a -> b -> a
c x y = x

c'' :: b -> a -> b
c'' x y = x

c' :: a -> b -> b
c' x y = y 

r :: [a] -> [a]
r list = list ++ list

ff :: a -> b
ff   = undefined

sf :: a -> b 
sf   = undefined

sign :: a
sign = undefined

co :: (b -> c) -> (a -> b) -> a -> c
co ff sf sign = ff (sf sign)

a :: (a -> c) -> a -> a
a _ x = x

a' :: (a -> b) -> a -> b
a' f x = f x 
