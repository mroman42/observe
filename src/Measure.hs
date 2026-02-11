{-# LANGUAGE RebindableSyntax #-}

module Measure 
  ( Measure (..)
  , measBind
  , measReturn
  , measNormalize
  , Distributional (..)
  , validity
  , uniform
  , totalWeight
  ) where

import Prelude
import Data.Ord
import Data.Maybe
import Data.List ( maximumBy )
import AuxiliarySemiring


class Distributional d where
  fromList :: (Eq a) => [(a, Rational)] -> d a
  toList :: (Eq a) => d a -> [(a, Rational)]
  weightOf :: (Eq a) => a -> d a -> Rational
  simplify :: (Eq a) => d a -> d a


validity :: (Distributional d, Eq a) => d a -> Rational
validity xs = sum $ map snd $ toList xs

uniform :: (Distributional d, Eq a) => [a] -> d a
uniform l = fromList $ map (\x -> (x, uniformWeight)) l
  where
    uniformWeight = 1 / (fromIntegral (length l))


newtype Measure a = Measure [(a, Rational)]

instance Distributional Measure where
  toList (Measure l) = l
  fromList l = Measure l
  weightOf x (Measure l) = weightOfPoint x l
  simplify (Measure l) = Measure $ reweight $ removeZeroes l

measFilterMaybe :: Measure (Maybe a) -> Measure a
measFilterMaybe (Measure xs) = Measure $ removeMaybes xs

instance (Eq a) => Eq (Measure a) where
  (==) :: (Eq a) => Measure a -> Measure a -> Bool
  (==) (Measure m) (Measure n) = isJust $ checkMaybe m n

measBind :: (Eq a, Eq b) => Measure a -> (a -> Measure b) -> Measure b
measBind (Measure xs) f = Measure $ sBind xs (toList . f)

measReturn :: (Eq a) => a -> Measure a
measReturn x = Measure [(x,1)]


