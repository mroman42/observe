module DistributeMaybeAffinebag where
    
import FinitaryMonad
import Bag
import AffineBag


distributiveMaybeBag :: (Eq a) => AffineBag (Maybe a) -> Maybe (AffineBag a)
distributiveMaybeBag b = case bagValidity b of
  0 -> Nothing
  n -> Just (AffineBag 
                (fMap unsafeFromMaybe (bagFilter ((/=) Nothing) 
                    (unAffineBag b))))

