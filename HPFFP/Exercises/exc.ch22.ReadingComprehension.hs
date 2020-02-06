{-# LANGUAGE InstanceSigs #-}

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader g) = Reader $ f . g

{- 1. Write liftA2 yourself. Think about it in terms of abstracting out
the diﬀerence between getDogR and getDogR' if that helps. -}
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 appl a b = appl <$> a <*> b

-- 2. Write the following function. Again, it is simpler than it looks.
asks :: (r -> a) -> Reader r a
asks f = Reader f

{- 3. Implement the Applicative for Reader.
To write the Applicative instance for Reader, we’ll use a pragma
called InstanceSigs. It’s an extension we need in order to assert
a type for the typeclass methods. You ordinarily cannot assert
type signatures in instances. The compiler already knows the
type of the functions, so it’s not usually necessary to assert the
types in instances anyway. We did this for the sake of clarity, to
make the Reader type explicit in our signatures. -}

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \r -> a
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)
