{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}



module AffineBag where

import Bag
import Prelude
import AuxiliarySemiring
import FinitaryMonad
import Data.Maybe
import Distribution
import BagDist

data AffineBag a = AffineBag (Bag a) deriving (Eq)

instance (Show a, Eq a) => Show (AffineBag a) where
  show (AffineBag x) = show x

unAffineBag :: AffineBag a -> Bag a
unAffineBag (AffineBag b) = b

mkAffineBag :: (Eq a) => [(a, Int)] -> AffineBag a
mkAffineBag m = case totalWeight m of
    0 -> error "Empty affine bag."
    _ -> AffineBag (Bag m)

bagValidity :: (Eq a) => AffineBag (Maybe a) -> Int
bagValidity (AffineBag b) = totalWeight (unBag (bagFilter ((/=) Nothing) b))


instance FinitaryMonad AffineBag where
  fBind :: (Eq a, Eq b) => AffineBag a -> (a -> AffineBag b) -> AffineBag b
  fBind (AffineBag (Bag d)) f = 
    AffineBag $ Bag $ 
    sBind d (unBag . unAffineBag . f)

  fReturn x = AffineBag $ Bag [(x,1)]

  fNext :: (Eq a, Eq b) => AffineBag a -> AffineBag b -> AffineBag b
  fNext d f = fBind d (const f)

  fMap :: (Eq a, Eq b) => (a -> b) -> AffineBag a -> AffineBag b
  fMap f (AffineBag u) = AffineBag $ fMap f u

unsafeFromMaybe :: Maybe a -> a
unsafeFromMaybe = fromMaybe undefined

distributeBagDist :: (Eq a) => 
  AffineBag (Distribution a) -> Distribution (AffineBag a)
distributeBagDist (AffineBag b) = fMap AffineBag $ distribute b
  
  
