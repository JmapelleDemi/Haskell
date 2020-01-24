-- Write an Applicative instance for Constant

newtype Constant a b = 
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure a                          = Constant mempty
  (<*>) (Constant a) (Constant b) = Constant (a `mappend` b)