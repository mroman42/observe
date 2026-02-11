{-# LANGUAGE RebindableSyntax #-}

module FinMonad.Bag 
  ( module FinMonad.FinMonad
  , module FinMonad.Bag
  ) where

import FinMonad.FinMonad
import Prelude hiding ((>>=), (>>), return, pure, map)
import Prelude qualified as P
import AuxiliarySemiring
import Data.Maybe

data Bag a where
    Bag :: (Eq a) => [(a, Int)] -> Bag a

unBag :: Bag a -> [(a, Int)]
unBag (Bag d) = d

bagWeight :: (Eq a) => [(a, Int)] -> Bag a
bagWeight = Bag . condense

bag :: (Eq a) => [a] -> Bag a
bag = Bag . condense . fmap (\x -> (x,1))

bagFilter :: (Eq a) => (a -> Bool) -> Bag a -> Bag a
bagFilter p (Bag l) = bagWeight (sFilter p l)


instance (Eq a) => Eq (Bag a) where
  (==) :: (Eq a) => Bag a -> Bag a -> Bool
  (==) (Bag m) (Bag n) = isJust $ checkMaybe (condense m) (condense n)

bagReturn :: (Eq a) => a -> Bag a
bagReturn x = Bag [(x,1)]

bagAdd :: (Eq a) => Bag a -> Bag a -> Bag a
bagAdd (Bag u) (Bag v) = bagWeight (u ++ v)

instance FinitaryMonad Bag where
  fBind :: (Eq a, Eq b) => Bag a -> (a -> Bag b) -> Bag b
  fBind (Bag d) f = bagWeight $ sBind d (unBag . f)

  fReturn x = bagWeight [(x,1)]

  fMap :: (Eq a, Eq b) => (a -> b) -> Bag a -> Bag b
  fMap f (Bag u) = bagWeight $ sMap f u

  
(>>=) :: (Eq a, Eq b) 
  => Bag a -> (a -> Bag b) -> Bag b
(>>=) = fBind

(>>) :: (Eq a, Eq b) 
  => Bag a -> Bag b -> Bag b
(>>) = fNext

return :: (Eq a) => a -> Bag a
return = fReturn

pure :: (Eq a) => a -> Bag a
pure = return

preshow (Bag []) = ""
preshow (Bag ((x,0):l)) = preshow (Bag l) 
preshow (Bag ((x,1):[])) = show x
preshow (Bag ((x,n):l)) = show x ++ "," ++ preshow (Bag ((x,n-1):l))

instance (Eq a, Show a) => Show (Bag a) where
  show :: (Eq a) => Bag a -> String
  show x = "(" ++ preshow x ++ ")"
