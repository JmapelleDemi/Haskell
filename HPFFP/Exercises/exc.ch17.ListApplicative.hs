{- Implement the List Applicative. Writing a minimally complete Ap-
plicative instance calls for writing the deﬁnitions of both pure and
<*>. We’re going to provide a hint as well. Use the checkers library to
validate your Applicative instance. -}

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Monoid (List a) where
  mempty                 = Nil
  mappend Nil x          = x
  mappend x Nil          = x
  mappend (Cons x xs) ys = Cons x $ xs <> ys

instance Semigroup (List a) where
  (<>) = mappend 

instance Functor List where
  fmap f Nil        = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)

instance Applicative List where
  pure a         = Cons a Nil
  Nil <*> _      = Nil
  _ <*> Nil      = Nil
  Cons f x <*> y = (f <$> y) <> (x <*> y)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as