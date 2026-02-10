module DistributeAffinebagDistribution where

import Bag
import AffineBag
import Distribution
import FinitaryMonad


distributeAffinebagDistribution :: (Eq a) =>
  Distribution (AffineBag a) -> AffineBag (Distribution a)
distributeAffinebagDistribution (Distribution p) =
  fMap Distribution (listAPack p)

listAPack :: (Eq a) => [((AffineBag a), Rational)] -> AffineBag [(a,Rational)]
listAPack [] = AffineBag $ bag [[]]
listAPack ((b,p):l) =
  fBind b (\x ->
    fBind (listAPack l) (\xs ->
        fReturn ((x,p):xs)))
  