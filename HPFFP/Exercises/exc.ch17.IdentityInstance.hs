newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure             = Identity
  Identity a <*> b = fmap a b 
