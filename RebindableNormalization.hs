{-# LANGUAGE RebindableSyntax #-}
module RebindableNormalization where

import SubdistributionAux
import Subdistribution hiding ((>>=), (>>), return)
import Normalization hiding ((>>=), (>>), return)
import Prelude hiding ((>>=), (>>), return)
import Data.Maybe

unsafeFromMaybe :: Maybe a -> a
unsafeFromMaybe = fromMaybe undefined

normD :: (Eq a) => Distribution (Maybe a) -> Maybe (Distribution a)
normD d = ifThenElse (validity d == 0) Nothing 
  (Just (dmap unsafeFromMaybe $ normFilter (/= Nothing) d))


(>>=) :: (Eq a, Eq b) => Maybe (Distribution a) -> (a -> Maybe (Distribution b)) -> Maybe (Distribution b)
(>>=) Nothing f = Nothing
(>>=) (Just d) f = fmap dJoin $ normD $ dmap f d

(>>) :: (Eq a, Eq b) => Maybe (Distribution a) -> Maybe (Distribution b) -> Maybe (Distribution b)
(>>) d f = d >>= const f

return :: (Eq a) => a -> Maybe (Distribution a)
return x = Just $ Distribution [(x,1)]



data HasGene = Gene | NoGene deriving (Eq, Show)
data HasTar = Tar | NoTar deriving (Eq, Show)
data IsSmoker = Smoker | NonSmoker deriving (Eq, Show)
data HasCancer = Cancer | NoCancer deriving (Eq, Show)

prevalence :: Maybe (Distribution HasGene)
prevalence = Just $ distribution [(Gene, 1/3), (NoGene, 2/3)]


smokes :: HasGene -> Maybe (Distribution IsSmoker)
smokes Gene = Just $ distribution [(Smoker, 3/4), (NonSmoker, 1/4)]
smokes NoGene = Just $ distribution [(Smoker, 1/4), (NonSmoker, 3/4)]

tar :: IsSmoker -> Maybe (Distribution HasTar)
tar Smoker = Just $ distribution [(Tar, 9/10), (NoTar, 1/10)]
tar NonSmoker = Just $ distribution [(Tar, 1/10), (NoTar, 9/10)]

health :: HasGene -> HasTar -> Maybe (Distribution HasCancer)
health Gene Tar = Just $ distribution [(Cancer, 3/5), (NoCancer, 2/5)]
health NoGene Tar = Just $ distribution [(Cancer, 2/5), (NoCancer, 3/5)]
health Gene NoTar = Just $ distribution [(Cancer, 3/10), (NoCancer, 7/10)]
health NoGene NoTar = Just $ distribution [(Cancer, 2/10), (NoCancer, 8/10)]

intervene :: Bool -> Maybe (Distribution ())
intervene True = return ()
intervene False = Just $ Distribution []


cancerDist :: Maybe (Distribution (HasCancer, IsSmoker))
cancerDist = do
    gene <- prevalence
    isSmoker <- smokes gene
    hasTar <- tar isSmoker
    cancer <- health gene hasTar
    return (cancer, isSmoker)


conditionalSmoking :: Maybe (Distribution HasCancer)
conditionalSmoking = do
    (gene, isSmoker) <- do 
        gene <- prevalence
        isSmoker <- smokes gene
        return (gene, isSmoker)
    intervene (isSmoker == Smoker)
    hasTar <- tar isSmoker
    cancer <- health gene hasTar
    return cancer

causalSmoking :: Maybe (Distribution HasCancer)
causalSmoking = do
    gene <- prevalence
    isSmoker <- smokes gene
    intervene (isSmoker == Smoker)
    hasTar <- tar isSmoker
    cancer <- health gene hasTar
    return cancer

    
-- (|>) :: (Eq a, Eq b) => Maybe (Distribution a) -> (a -> Maybe (Distribution b)) -> Maybe (Distribution b)
-- (|>) = (>>=)

-- conditionalSmokingB :: Maybe (Distribution HasCancer)
-- conditionalSmokingB = 
--     prevalence >>= \gene ->
--     smokes gene >>= \isSmoker ->
--     guarding (isSmoker == Smoker) >>= \() ->
--     tar Smoker >>= \hasTar ->
--     health gene hasTar >>= \cancer ->
--     return cancer

-- causalSmokingB :: Maybe (Distribution HasCancer)
-- causalSmokingB = 
--     prevalence |> \gene ->
--     (smokes gene |> \isSmoker ->
--     (guarding (isSmoker == Smoker) |> \() ->
--     (tar Smoker |> \hasTar ->
--     health gene hasTar |> \cancer ->
--     return cancer)))

-- conditionalSmokingA :: Maybe (Distribution HasCancer)
-- conditionalSmokingA = norm $ do
--     gene <- prevalence
--     isSmoker <- smokes gene
--     observe (isSmoker == Smoker)
--     hasTar <- tar isSmoker
--     cancer <- health gene hasTar
--     return cancer

-- causalSmoking :: Maybe (Distribution HasCancer)
-- causalSmoking = norm $ do
--     gene <- prevalence
--     isSmoker <- normalizationBox $ do 
--         isSmoker <- smokes gene
--         observe (isSmoker == Smoker)
--         return isSmoker
--     hasTar <- tar isSmoker
--     cancer <- health gene hasTar        
--     return cancer

-- causalSmokingB :: Maybe (Distribution HasCancer)
-- causalSmokingB = norm $ do
--     gene <- prevalence
--     isSmoker <- return Smoker
--     hasTar <- tar isSmoker
--     cancer <- health gene hasTar        
--     return cancer    


