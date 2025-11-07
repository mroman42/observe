module FinitaryMonad where

class FinitaryMonad m where    
    fBind :: (Eq a, Eq b) => m a -> (a -> m b) -> m b
    fNext :: (Eq a, Eq b) => m a -> m b -> m b
    fReturn :: (Eq a) => a -> m a
    fMap :: (Eq a, Eq b) => (a -> b) -> m a -> m b

fJoin :: (FinitaryMonad m, Eq a, Eq (m a)) => m (m a) -> m a
fJoin xs = fBind xs id