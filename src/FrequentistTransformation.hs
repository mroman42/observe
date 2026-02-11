module FrequentistTransformation where

import AffineBag
import Bag
import Distribution
import AuxiliarySemiring 

flrn :: (Eq a) => AffineBag a -> Distribution a  
flrn (AffineBag (Bag d)) = Distribution $ valueMap 
  (\v -> (fromIntegral v) / (fromIntegral (totalWeight d))) d
