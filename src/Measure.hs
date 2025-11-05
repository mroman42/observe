{-# LANGUAGE RebindableSyntax #-}

module Measure 
  ( Measure (..)
  , measBind
  , measReturn
  , Distributional (..)
  , validity
  , uniform
  ) where

import Prelude
import Data.Ord
import Data.Maybe
import Data.List ( maximumBy )

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



removeZeroes :: [(a, Rational)] -> [(a, Rational)]
removeZeroes [] = []
removeZeroes ((x,r):l)
 | r == 0    = removeZeroes l
 | otherwise = (x,r) : removeZeroes l

weightOfPoint :: (Eq a) => a -> [(a,Rational)] -> Rational
weightOfPoint x [] = 0
weightOfPoint x ((y,r) : l) 
 | x == y    = r + weightOfPoint x l
 | otherwise = weightOfPoint x l

reweight :: (Eq a) => [(a,Rational)] -> [(a,Rational)]
reweight [] = []
reweight ((x,r) : l) = 
  let w  = r + weightOfPoint x l 
      lw = reweight (filter (\(y,s) -> x /= y) l)
  in ifThenElse (w == 0) lw ((x,w):lw)
 where
  ifThenElse True  x y = x
  ifThenElse False x y = y

condense :: (Eq a) => [(a,Rational)] -> [(a,Rational)]
condense = reweight . removeZeroes

checkCoincideThis :: (Eq a) => (a,Rational) -> [(a,Rational)] -> Bool
checkCoincideThis (x,r) [] = False
checkCoincideThis (x,r) ((y,s):l) = ((x == y) && (r == s)) || checkCoincideThis (x,r) l

checkMaybeThis :: (Eq a) => (a,Rational) -> [(a,Rational)] -> Maybe [(a,Rational)]
checkMaybeThis (x,r) [] = Nothing
checkMaybeThis (x,r) ((y,s):u) | (x == y) && (r == s) = Just u
                               | otherwise = do
                                    v <- checkMaybeThis (x,r) u
                                    return ((y,s) : v)

checkMaybe :: (Eq a) => [(a,Rational)] -> [(a,Rational)] -> Maybe ()
checkMaybe [] [] = return ()
checkMaybe [] v  = return ()
checkMaybe ((x,r):u) v = do
  w <- checkMaybeThis (x,r) v
  checkMaybe u w

distMap :: (a -> b) -> [(a,Rational)] -> [(b,Rational)]
distMap f [] = []
distMap f ((x,r):l) = (f x, r) : distMap f l

distJoin :: (Eq a) => [([(a,Rational)],Rational)] -> [(a,Rational)]
distJoin = condense . concatMap (\(u, s) -> map (\(x,r) -> (x, r * s)) u)

wlBind :: (Eq a, Eq b) => [(a, Rational)] -> (a -> [(b, Rational)]) -> [(b, Rational)]
wlBind d f = distJoin $ distMap f d

measBind :: (Eq a, Eq b) => Measure a -> (a -> Measure b) -> Measure b
measBind (Measure xs) f = Measure $ wlBind xs (toList . f)

measReturn :: (Eq a) => a -> Measure a
measReturn x = Measure [(x,1)]

totalWeight :: [(a,Rational)] -> Rational
totalWeight l = sum (map snd l)

distNormalize :: (Eq a) => [(a,Rational)] -> [(a,Rational)]
distNormalize l = map (\(x,r) -> (x,r / totalWeight l)) l


