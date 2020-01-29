data Validation e a = Failure e | Success a deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f (Failure e) = Failure e
  fmap f (Success a) = Success $ f a

instance Monoid e => Applicative (Validation e) where
  pure a                        = Success a
  (<*>) _ (Failure e)           = Failure e
  (<*>) (Failure e) _           = Failure e
  (<*>) (Success a) (Success b) = Success $ a b