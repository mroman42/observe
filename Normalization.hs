{-# LANGUAGE RebindableSyntax #-}

module Normalization where


import Data.Maybe
import Subdistribution
import Prelude hiding ((>>=), (>>), return)

ifThenElse True  x y = x
ifThenElse False x y = y

normD :: (Eq a) => Distribution (Maybe a) -> Maybe (Distribution a)
normD d = ifThenElse (weightOf Nothing d == 1)
        Nothing 
        (Just (dmap (fromMaybe undefined) $ normFilter (/= Nothing) d))

norm :: (Eq a) => Distribution a -> Maybe (Distribution a)
norm d = ifThenElse (validity d == 0) Nothing (Just (normalize d))

subd :: (Eq a) => Maybe (Distribution a) -> Distribution a
subd = fromMaybe (Distribution [])

normalizationBox :: (Eq a) => Distribution a -> Distribution a
normalizationBox = subd . norm

data HasGene = Gene | NoGene deriving (Eq, Show)
data HasTar = Tar | NoTar deriving (Eq, Show)
data IsSmoker = Smoker | NonSmoker deriving (Eq, Show)
data HasCancer = Cancer | NoCancer deriving (Eq, Show)

prevalence :: Distribution HasGene
prevalence = distribution [(Gene, 30/100), (NoGene, 40/100)]

smokes :: HasGene -> Distribution IsSmoker
smokes Gene = distribution [(Smoker, 6/10), (NonSmoker, 4/10)]
smokes NoGene = distribution [(Smoker, 2/10), (NonSmoker, 8/10)]

health :: HasGene -> IsSmoker -> Distribution HasCancer
health Gene Smoker = distribution [(Cancer, 6/10), (NoCancer, 4/10)]
health NoGene Smoker = distribution [(Cancer, 4/10), (NoCancer, 6/10)]
health Gene NonSmoker = distribution [(Cancer, 3/10), (NoCancer, 7/10)]
health NoGene NonSmoker = distribution [(Cancer, 2/10), (NoCancer, 8/10)]

conditionalSmoking :: Maybe (Distribution HasCancer)
conditionalSmoking = norm $ do
    gene <- prevalence
    isSmoker <- smokes gene
    observe (isSmoker == Smoker)
    cancer <- health gene isSmoker
    return cancer

causalSmoking :: Maybe (Distribution HasCancer)
causalSmoking = norm $ do
    gene <- prevalence
    isSmoker <- normalizationBox $ do 
        isSmoker <- smokes gene
        observe (isSmoker == Smoker)
        return isSmoker
    cancer <- health gene isSmoker
    return cancer