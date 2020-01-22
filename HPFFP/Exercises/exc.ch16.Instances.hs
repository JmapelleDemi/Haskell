{-
Implement Functor instances for the following datatypes. Use the
QuickCheck properties we just showed you to validate them.

1. newtype Identity a = Identity a -}
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

-- 2. data Pair a = Pair a a
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

-- 3. data Two a b = Two a b 
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

-- 4. data Three a b c = Three a b 
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

-- 5. data Three' a b = Three' a b b
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)
  
-- 6. data Four a b c d = Four a b c d
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

-- 7. data Four' a b = Four' a a a b
data Four' a b = Four' a a a b deriving (Eq, Show)
  
instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)
