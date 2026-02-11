module FrequentistTransformation where

import FinMonad.AffineBag
import FinMonad.Bag
import FinMonad.Distribution
import AuxiliarySemiring 

flrn :: (Eq a) => AffineBag a -> Distribution a  
flrn (AffineBag (Bag d)) = Distribution $ valueMap 
  (\v -> (fromIntegral v) / (fromIntegral (totalWeight d))) d
