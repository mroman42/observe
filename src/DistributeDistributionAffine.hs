module DistributeDistributionAffine where


import Distribution
import AffineBag
import FinitaryMonad
import BagDist

distributeDistributionAffine 
  :: (Eq a) => AffineBag (Distribution a) -> Distribution (AffineBag a)
distributeDistributionAffine (AffineBag p) = 
  fMap AffineBag $ distribute p

