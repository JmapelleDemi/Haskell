import Data.Monoid
{- Implement the functions in terms of foldMap or foldr from Foldable,
then try them out with multiple types that have Foldable instances. 

1. This and the next one are nicer with foldMap, but foldr is ﬁne
too. -}
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum 
-- 2.
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product
-- 3.
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = getAny . foldMap (\y-> Any (x == y))
-- 4.  
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' =
  foldr
  (\x y -> case y of Nothing -> Just x
                     Just z  -> if x < z then Just x else Just z)
  Nothing

-- 5.
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' =
  foldr
  (\x y -> case y of Nothing -> Just x
                     Just z  -> if x < z then Just z else Just x)
  Nothing
-- 6.
null' :: (Foldable t) => t a -> Bool
null' = foldr (\x y -> False) True
--7.
length' :: (Foldable t) => t a -> Int
length' = getSum . foldMap (\x -> Sum 1)
-- 8. Some say this is all Foldable amounts to.
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []
-- 9. Hint: use foldMap. Combine the elements of a structure using a monoid.
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id
-- 10. Deﬁne foldMap in terms of foldr.
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f fl = foldr (\x y -> (f x) `mappend` y) mempty fl