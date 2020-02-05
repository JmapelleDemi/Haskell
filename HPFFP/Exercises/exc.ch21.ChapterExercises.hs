{- Write a Traversable instance for the datatype provided, ﬁlling in any
required superclasses -}
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a 

instance Foldable Identity where
  foldMap f (Identity a) = f a
  foldr f z (Identity a) = f a z
 
instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse f (Constant x) = pure $ Constant x

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
  fmap f Nada    = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap f Nada    = mempty
  foldMap f (Yep a) = f a
  foldr f z Nada    = z
  foldr f z (Yep a) = f a z

instance Traversable Optional where
  traverse f Nada    = pure Nada
  traverse f (Yep a) = Yep <$> f a

data List  a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap f Nil        = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)

instance Foldable List where
  foldMap f Nil        = mempty
  foldMap f (Cons a b) = (f a) <> (foldMap f b)
  foldr f z Nil        = z
  foldr f z (Cons a b) = f a $ foldr f z b

instance Traversable List where
  traverse f Nil        = pure Nil
  traverse f (Cons a b) = Cons <$> f a <*> traverse f b

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c
  foldr f z (Three a b c) = f c z

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c
  sequenceA  (Three a b c) = (Three a b) <$> c

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Foldable (Three' a) where
  foldMap f (Three' a b c) = (f b) <> (f c)

instance Traversable (Three' a) where
  traverse f (Three' a b c) = (Three' a) <$> (f b) <*> (f c)
  sequenceA  (Three' a b c) = (Three' a) <$> b <*> c

data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S a b) = S (fmap f a) (f b)

instance Foldable n => Foldable (S n) where
   foldMap f (S n a) = foldMap f n <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S a b) = S <$> traverse f a <*> f b

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap f Empty        = Empty
  fmap f (Leaf a)     = Leaf $ f a
  fmap f (Node a b c) = Node (fmap f a) (f b) (fmap f c)

instance Foldable Tree where
  foldMap f Empty        = mempty
  foldMap f (Leaf a)     = f a
  foldMap f (Node a b c) = (foldMap f a) <> (f b) <> (foldMap f c)
{-foldr f z Empty          = mempty
  foldr f z (Leaf a)       = f a z
  foldr f z (Node a b c)   = (foldr f z a) <> (f b) <> (foldr f z c) -}

instance Traversable Tree where
  traverse f Empty        = pure Empty
  traverse f (Leaf a)     = Leaf <$> (f a)
  traverse f (Node a b c) = Node <$> traverse f a <*> f b <*> traverse f c