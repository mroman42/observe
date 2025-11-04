{-# LANGUAGE RebindableSyntax #-}
module ExampleSmoking where

import Data.Maybe
import Subdistribution
import Prelude hiding ((>>=), (>>), return)
import NormalizationBoxes    

data HasGene = Gene | NoGene deriving (Eq, Show)
data HasTar = Tar | NoTar deriving (Eq, Show)
data IsSmoker = Smoker | NonSmoker deriving (Eq, Show)
data HasCancer = Cancer | NoCancer deriving (Eq, Show)

prevalence :: Distribution HasGene
prevalence = distribution [(Gene, 1/3), (NoGene, 2/3)]

smokes :: HasGene -> Distribution IsSmoker
smokes Gene = distribution [(Smoker, 3/4), (NonSmoker, 1/4)]
smokes NoGene = distribution [(Smoker, 1/4), (NonSmoker, 3/4)]

tar :: IsSmoker -> Distribution HasTar
tar Smoker = distribution [(Tar, 9/10), (NoTar, 1/10)]
tar NonSmoker = distribution [(Tar, 1/10), (NoTar, 9/10)]

health :: HasGene -> HasTar -> Distribution HasCancer
health Gene Tar = distribution [(Cancer, 3/5), (NoCancer, 2/5)]
health NoGene Tar = distribution [(Cancer, 2/5), (NoCancer, 3/5)]
health Gene NoTar = distribution [(Cancer, 3/10), (NoCancer, 7/10)]
health NoGene NoTar = distribution [(Cancer, 2/10), (NoCancer, 8/10)]

cancerDist = do
    gene <- prevalence
    isSmoker <- smokes gene
    hasTar <- tar isSmoker
    cancer <- health gene hasTar
    return (cancer, isSmoker)

conditionalSmoking :: Distribution HasCancer
conditionalSmoking = do
    gene <- prevalence
    isSmoker <- smokes gene
    observe (isSmoker == Smoker)
    hasTar <- tar isSmoker
    cancer <- health gene hasTar
    return cancer

conditionalSmokingA :: Distribution HasCancer
conditionalSmokingA = normalizationBox $ do
    gene <- prevalence
    isSmoker <- smokes gene
    observe (isSmoker == Smoker)
    hasTar <- tar isSmoker
    cancer <- health gene hasTar
    return cancer

causalSmoking :: Distribution HasCancer
causalSmoking = normalizationBox $ do
    gene <- prevalence
    isSmoker <- normalizationBox $ do 
        isSmoker <- smokes gene
        observe (isSmoker == Smoker)
        return isSmoker
    hasTar <- tar isSmoker
    cancer <- health gene hasTar        
    return cancer

causalSmokingB :: Distribution HasCancer
causalSmokingB = normalizationBox $ do
    gene <- prevalence
    isSmoker <- return Smoker
    hasTar <- tar isSmoker
    cancer <- health gene hasTar        
    return cancer    
