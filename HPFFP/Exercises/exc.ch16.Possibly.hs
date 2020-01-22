{- Write a Functor instance for a datatype identical to Maybe. We’ll use
our own datatype because Maybe already has a Functor instance and
we cannot make a duplicate one. -}

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
  fmap f LolNope = LolNope
  fmap f (Yeppers a) = Yeppers $ f a

-- If it helps, you’re basically writing the following function:
-- applyIfJust :: (a -> b) -> Maybe a -> Maybe b

applyPoss :: (a -> b) -> Possibly a -> Possibly b
applyPoss f smth = fmap f smth