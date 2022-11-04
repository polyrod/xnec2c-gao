module Utils where

import Types
import Data.These
import Data.Text (Text)

hasFitness :: Phenotype -> Bool
hasFitness (Phenotype (This (PhenotypeData _ (Fitness _ _ _)))) = True
hasFitness (Phenotype (That bpm)) = any (\(PhenotypeData _ f) -> case f of
                                                                    Fitness _ _ _ -> True
                                                                    _ -> False) bpm
hasFitness (Phenotype (These _ bpm)) = any (\(PhenotypeData _ f) -> case f of
                                                                    Fitness _ _ _ -> True
                                                                    _ -> False) bpm
hasFitness _ = False

getFitness :: Phenotype -> Fitness
getFitness (Phenotype (This (PhenotypeData _ f@(Fitness vswr gain fbr)))) = f
getFitness (Phenotype (This (PhenotypeData _ None))) = None 
getFitness (Phenotype (That bmd)) = let f@(Fitness a b c) = foldr (<>) None $ fmap fitness bmd in f 
getFitness (Phenotype (These frd bmd)) = let f@(Fitness a b c) = foldr (<>) None $ fmap fitness bmd in f

 
outputPhenotype :: Phenotype -> Text
outputPhenotype (Phenotype (This (PhenotypeData d _))) = d
outputPhenotype (Phenotype (These (PhenotypeData d _) _)) = d
