import Prelude hiding (Left, Right)
import Control.Monad
{- Write Monad instances for the following types

1. -}
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap f NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure a = NopeDotJpg
  (<*>) NopeDotJpg NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return             = pure
  (>>=) NopeDotJpg a = NopeDotJpg

-- 2. 
data PhbtEither b a = Left a | Right b deriving (Eq, Show)

instance Functor (PhbtEither a) where
  fmap f (Left a)  = Left $ f a
  fmap f (Right a) = Right a
 
instance (Monoid a) => Applicative (PhbtEither a) where
  pure                    = Left
  (<*>) (Right a) _       = Right a
  (<*>) (Left a) (Left b) = Left $ a b
  (<*>) (Left a) b        = fmap a b

instance (Monoid a) => Monad (PhbtEither a) where
  return = pure
  (>>=) (Right a) b = Right a
  (>>=) (Left a) b  = b a

-- 3. Write a Monad instance for Identity
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure                            = Identity
  (<*>) (Identity a) (Identity b) = Identity $ a b

instance Monad Identity where
  return = pure
  (>>=) (Identity a) am = am a

-- 4.
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap f Nil        = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)

instance Semigroup (List a) where
  (<>) Nil (Cons a b)        = Cons a b
  (<>) (Cons a b) Nil        = Cons a b
  (<>) (Cons a b) (Cons c d) = Cons a (b <> (Cons c d))
-- doesn't work without Semigroup instances

instance Monoid (List a) where
  mempty                 = Nil
  mappend a Nil          = a
  mappend Nil a          = a
  mappend (Cons a Nil) b = Cons a b
  mappend (Cons a b) c   = Cons a $ mappend b c

instance Applicative List where
  pure a              = Cons a Nil 
  (<*>) Nil _         = Nil
  (<*>) _ Nil         = Nil
  (<*>) (Cons a b ) c = mappend (fmap a c) (b <*> c)

instance Monad List where
  return              = pure 
  (>>=) Nil        am = Nil
  (>>=) (Cons a b) am = mappend (am a) ( (>>=) b am )

{- Write the following functions using the methods provided by
Monad and Functor. Using stuﬀ like identity and composition is ﬁne,
but it has to typecheck with types provided.

1. -}
j :: Monad m => m (m a) -> m a
j a = join a

-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f a = fmap f a

-- 3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = f <$> a <*> b

-- 4.
a :: Monad m => m a -> m (a -> b) -> m b
a sa sf = sf <*> sa 

-- 5. You’ll need recursion for this one.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh (x:[]) f = (:[]) <$> (f x)
meh (x:xs) f = (:) <$> (f x) <*> (meh xs f)

-- 6. Hint: reuse “meh”
flipType :: (Monad m) => [m a] -> m [a]
flipType list = meh list id