{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}

module AuxiliarySemiring where

import Prelude
import Data.Ord
import Data.Maybe
import Data.List ( maximumBy )




removeMaybes :: (Num r) => [(Maybe a, r)] -> [(a, r)]
removeMaybes [] = []
removeMaybes ((Just x, v):l) = (x,v) : removeMaybes l
removeMaybes ((Nothing, v):l) = removeMaybes l

removeZeroes :: (Num r, Eq r) => [(a, r)] -> [(a, r)]
removeZeroes [] = []
removeZeroes ((x,r):l)
 | r == 0    = removeZeroes l
 | otherwise = (x,r) : removeZeroes l

weightOfPoint :: (Eq a, Num r) => a -> [(a, r)] -> r
weightOfPoint x [] = 0
weightOfPoint x ((y,r) : l) 
 | x == y    = r + weightOfPoint x l
 | otherwise = weightOfPoint x l

totalWeight :: (Num r) => [(a, r)] -> r
totalWeight l = sum (map snd l)
 
reweight :: (Eq a, Eq r, Num r) => [(a, r)] -> [(a, r)]
reweight [] = []
reweight ((x,r) : l) = 
  let w  = r + weightOfPoint x l 
      lw = reweight (filter (\(y,s) -> x /= y) l)
  in ifThenElse (w == 0) lw ((x,w):lw)
 where
  ifThenElse True  x y = x
  ifThenElse False x y = y

condense :: (Eq a, Num r, Eq r) => [(a, r)] -> [(a, r)]
condense = reweight . removeZeroes

checkCoincideThis :: (Eq a, Eq r) => (a, r) -> [(a, r)] -> Bool
checkCoincideThis (x,r) [] = False
checkCoincideThis (x,r) ((y,s):l) = 
       ((x == y) && (r == s)) 
    || checkCoincideThis (x,r) l


checkMaybeThis :: (Eq a, Eq r) => (a, r) -> [(a, r)] -> Maybe [(a, r)]
checkMaybeThis (x,r) [] = Nothing
checkMaybeThis (x,r) ((y,s):u) | (x == y) && (r == s) = Just u
                               | otherwise = do
                                    v <- checkMaybeThis (x,r) u
                                    return ((y,s) : v)

checkMaybe :: (Eq a, Eq r) => [(a, r)] -> [(a, r)] -> Maybe ()
checkMaybe [] [] = return ()
checkMaybe [] v  = Nothing
checkMaybe ((x,r):u) v = do
  w <- checkMaybeThis (x,r) v
  checkMaybe u w

sFilter :: (Num r) => (a -> Bool) -> [(a,r)] -> [(a,r)]
sFilter p [] = []
sFilter p ((x,r) : l) = case p x of
  True ->  ((x,r) : sFilter p l)
  False -> sFilter p l

sMap :: (Num r) => (a -> b) -> [(a, r)] -> [(b, r)]
sMap f [] = []
sMap f ((x,r):l) = (f x, r) : sMap f l

sJoin :: (Eq a, Eq r, Num r) => [([(a, r)], r)] -> [(a, r)]
sJoin = condense . concatMap (\(u, s) -> map (\(x,r) -> (x, r * s)) u)

sBind :: (Eq a, Eq b, Eq r, Num r) => [(a, r)] -> (a -> [(b, r)]) -> [(b, r)]
sBind d f = sJoin $ sMap f d

valueMap :: (r -> s) -> [(a, r)] -> [(a, s)]
valueMap f [] = []
valueMap f ((x, r):l) = (x, f r) : valueMap f l