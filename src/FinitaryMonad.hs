module FinitaryMonad where

class FinitaryMonad m where    
    (>>=) :: (Eq a, Eq b) => m a -> (a -> m b) -> m b
    (>>) :: (Eq a, Eq b) => m a -> m b -> m b
    return :: (Eq a) => a -> m a
    fMap :: (Eq a, Eq b) => (a -> b) -> m a -> m b
