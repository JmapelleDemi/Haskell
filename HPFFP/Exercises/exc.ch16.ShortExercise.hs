{- 1. Write a Functor instance for a datatype identical to Either. We’ll
use our own datatype because Either also already has a Functor
instance. -}

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a)  =  First a
  fmap f (Second b) = Second $ f b