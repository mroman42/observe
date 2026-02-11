module FrequentistTransformation where

import FinMonad.AffineBag
import FinMonad.Bag
import FinMonad.Distribution
import AuxiliarySemiring 
import FinMonad.NormalizedDistribution (Normalized(Normalized))

flrn :: (Eq a) => AffineBag a -> Distribution a  
flrn (AffineBag (Bag d)) = Distribution $ valueMap 
  (\v -> (fromIntegral v) / (fromIntegral (totalWeight d))) d

flrnNorm :: (Eq a) => Bag a -> Normalized a
flrnNorm (Bag b) = case totalWeight b of
    0 -> Normalized Nothing
    w -> Normalized $ Just $ Distribution $ valueMap 
            (\v -> (fromIntegral v) / (fromIntegral (totalWeight b))) b