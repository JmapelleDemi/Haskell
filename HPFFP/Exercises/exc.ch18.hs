{- Write bind in terms of fmap and join
join :: Monad m => m (m a) -> m a
fmap :: Functor f => (a -> f b) -> f a -> f (f b) -}
import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f a = join . fmap f $ a