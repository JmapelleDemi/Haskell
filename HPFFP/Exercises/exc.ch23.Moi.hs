{-# LANGUAGE InstanceSigs #-}

import Control.Arrow (first)

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

-- Implement the Functor instance for State.

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi (\x -> (f $ fst (g x), x))

-- Write the Applicative instance for State.

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (\s -> (a, s))
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (<*>) (Moi f) (Moi g) = Moi $ \s -> let (h, _) = f s
                                          (a, _) = g s
                                      in (h a, s)

-- Write the Monad instance for State

instance Monad (Moi s) where
  return = pure
  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s -> let (a, s') = f s
                              in runMoi (g a) s'