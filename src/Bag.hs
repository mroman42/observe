{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}


module Bag where

import Prelude hiding ((>>=), (>>), return, pure, map)
import Prelude qualified as P
import AuxiliarySemiring
import Data.Maybe
import FinitaryMonad

data Bag a where
    Bag :: (Eq a) => [(a, Int)] -> Bag a

unBag :: Bag a -> [(a, Int)]
unBag (Bag d) = d

bag :: (Eq a) => [(a, Int)] -> Bag a
bag = Bag . condense

bagFilter :: (Eq a) => (a -> Bool) -> Bag a -> Bag a
bagFilter p (Bag l) = bag (sFilter p l)


instance (Eq a) => Eq (Bag a) where
  (==) :: (Eq a) => Bag a -> Bag a -> Bool
  (==) (Bag m) (Bag n) = isJust $ checkMaybe (condense m) (condense n)

bagReturn :: (Eq a) => a -> Bag a
bagReturn x = Bag [(x,1)]

bagAdd :: (Eq a) => Bag a -> Bag a -> Bag a
bagAdd (Bag u) (Bag v) = bag (u ++ v)

instance FinitaryMonad Bag where
  fBind :: (Eq a, Eq b) => Bag a -> (a -> Bag b) -> Bag b
  fBind (Bag d) f = bag $ sBind d (unBag . f)

  fReturn x = bag [(x,1)]

  fNext :: (Eq a, Eq b) => Bag a -> Bag b -> Bag b
  fNext d f = fBind d (const f)

  fMap :: (Eq a, Eq b) => (a -> b) -> Bag a -> Bag b
  fMap f (Bag u) = bag $ sMap f u


preshow (Bag []) = ""
preshow (Bag ((x,0):l)) = preshow (Bag l)
preshow (Bag ((x,n):l)) = show x ++ preshow (Bag ((x,n-1):l))

instance (Eq a, Show a) => Show (Bag a) where
  show :: (Eq a) => Bag a -> String
  show x = "(" ++ preshow x ++ ")"
