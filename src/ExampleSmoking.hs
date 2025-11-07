{-# LANGUAGE RebindableSyntax #-}

module ExampleSmoking where

import Prelude hiding ((>>=), (>>), return)
import NormalizedDistribution

data HasGene = Gene | NoGene deriving (Eq, Show)
data HasTar = Tar | NoTar deriving (Eq, Show)
data IsSmoker = Smoker | NonSmoker deriving (Eq, Show)
data HasCancer = Cancer | NoCancer deriving (Eq, Show)

prevalence :: Normalized HasGene
prevalence = distribution [(Gene, 1/3), (NoGene, 2/3)]


smokes :: HasGene -> Normalized IsSmoker
smokes Gene = distribution [(Smoker, 3/4), (NonSmoker, 1/4)]
smokes NoGene = distribution [(Smoker, 1/4), (NonSmoker, 3/4)]

tar :: IsSmoker -> Normalized HasTar
tar Smoker = distribution [(Tar, 9/10), (NoTar, 1/10)]
tar NonSmoker = distribution [(Tar, 1/10), (NoTar, 9/10)]

health :: HasGene -> HasTar -> Normalized HasCancer
health Gene Tar = distribution [(Cancer, 3/5), (NoCancer, 2/5)]
health NoGene Tar = distribution [(Cancer, 2/5), (NoCancer, 3/5)]
health Gene NoTar = distribution [(Cancer, 3/10), (NoCancer, 7/10)]
health NoGene NoTar = distribution [(Cancer, 2/10), (NoCancer, 8/10)]


cancerDist :: Normalized (HasCancer, IsSmoker)
cancerDist = do
    gene <- prevalence
    isSmoker <- smokes gene
    hasTar <- tar isSmoker
    cancer <- health gene hasTar
    return (cancer, isSmoker)
--- >>> cancerDist
-- Just <Distribution> [((Cancer,Smoker),247 % 1200),((NoCancer,Smoker),253 % 1200),((Cancer,NonSmoker),11 % 80),((NoCancer,NonSmoker),107 % 240)]

conditionalSmoking :: Normalized HasCancer
conditionalSmoking = do
    (gene, isSmoker) <- do 
        gene <- prevalence
        isSmoker <- smokes gene
        return (gene, isSmoker)
    intervene (isSmoker == Smoker)
    hasTar <- tar isSmoker
    cancer <- health gene hasTar
    return cancer
--- >>> conditionalSmoking
-- Just <Distribution> [(Cancer,247 % 500),(NoCancer,253 % 500)]

causalSmoking :: Normalized HasCancer
causalSmoking = do
    gene <- prevalence
    isSmoker <- smokes gene
    intervene (isSmoker == Smoker)
    hasTar <- tar isSmoker
    cancer <- health gene hasTar
    return cancer
--- >>> causalSmoking
-- Just <Distribution> [(Cancer,133 % 300),(NoCancer,167 % 300)]
