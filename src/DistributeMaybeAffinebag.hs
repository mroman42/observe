module DistributeMaybeAffinebag where
    
import FinMonad
import FinMonad.Bag
import FinMonad.AffineBag


distributiveMaybeBag :: (Eq a) => AffineBag (Maybe a) -> Maybe (AffineBag a)
distributiveMaybeBag b = case bagValidity b of
  0 -> Nothing
  n -> Just (AffineBag 
                (fMap unsafeFromMaybe (bagFilter ((/=) Nothing) 
                    (unAffineBag b))))

