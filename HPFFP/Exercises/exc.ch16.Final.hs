{-# LANGUAGE FlexibleInstances #-}
{- Rearrange the arguments to the type constructor of the datatype
so the Functor instance works.
1. -} 
data Sum b a = First a | Second b deriving (Eq, Show)

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

--2.
data Company a b c = DeepBlue a b | Something c deriving (Eq, Show)

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

--3.
data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b

{- Write Functor instances for the following datatypes.
1. -}
data Quant a b = Finance
               | Desk a
               | Bloor b deriving (Eq, Show)

instance Functor (Quant a) where
  fmap f (Desk a)  = Desk a
  fmap f (Bloor a) = Bloor $ f a 

-- 2.
-- data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap f (K a) = K a

-- 3.
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K a b = K a deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K $ f a

-- 4.
data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst a) = GoatyConst $ f a

-- 5. 
data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut a) = LiftItOut $ fmap f a

-- 6.
data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa x y) = DaWrappa (fmap f x)  (fmap f y)

-- 7.
data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething x y) = IgnoringSomething x $ fmap f y

-- 8.
data Notorious g o a t = Notorious (g a) (g o) (g t) deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where 
  fmap f (Notorious x y z) = Notorious x y $ fmap f z

-- 9.
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap f Nil        = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)

-- 10.
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) deriving (Eq, Show)

instance Functor GoatLord where
  fmap f NoGoat             = NoGoat
  fmap f (OneGoat a)        = OneGoat $ f a 
  fmap f (MoreGoats a b c ) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

-- 11.
data TalkToMe a = Halt | Print String a | Read (String -> a)
  --deriving (Eq, Show)

instance Functor TalkToMe where
  fmap f Halt             = Halt
  fmap f (Print str a)    = Print str $ f a
  fmap f (Read x)         = Read (f . x)