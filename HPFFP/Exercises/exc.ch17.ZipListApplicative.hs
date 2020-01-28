{- Implement the ZipList Applicative. Use the checkers library to vali-
date your Applicative instance. We’re going to provide the EqProp
instance and explain the weirdness in a moment. -}
import Test.QuickCheck.Checkers

data List a = Nil | Cons a (List a) deriving (Eq, Show)

take' :: Int -> List a -> List a
take' _ Nil           = Nil
take' 1 (Cons a rest) = Cons a Nil
take' n (Cons a rest) = Cons a (take' (n-1) rest)

instance Monoid (List a) where
  mempty                 = Nil
  mappend Nil x          = x
  mappend x Nil          = x
  mappend (Cons x xs) ys = Cons x $ xs <> ys

instance Functor List where
  fmap f Nil           = Nil
  fmap f (Cons a rest) = Cons (f a) (fmap f rest) 

instance Semigroup (List a) where
  (<>) = mappend 

instance Applicative List where
  pure a                 = Cons a Nil
  Nil <*> _              = Nil
  _ <*> Nil              = Nil
  (<*>) (Cons f frest) a = (fmap f a) <> (frest <*> a)

newtype ZipList' a =
  ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l 
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a                                        = ZipList' (pure a)
  ZipList' Nil <*> _                            = ZipList' Nil
  _            <*> ZipList' Nil                 = ZipList' Nil
  ZipList' (Cons f fs) <*> ZipList' (Cons a as) = ZipList' (Cons (f a) (zipL fs as))

zipL :: List (a -> b) -> List a -> List b
zipL Nil _                   = Nil
zipL _ Nil                   = Nil
zipL (Cons f fs) (Cons a as) = Cons (f a) (zipL fs as)

repeat' :: Integer -> List Integer
repeat' n = Cons n (repeat' n)