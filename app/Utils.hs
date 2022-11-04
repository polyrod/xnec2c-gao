module Utils where

import Data.Text (Text)
import Data.These
import Types

hasFitness :: Phenotype -> Bool
hasFitness (Phenotype (This (PhenotypeData _ (Fitness {})))) = True
hasFitness (Phenotype (That bpm)) =
  any
    ( \(PhenotypeData _ f) -> case f of
        Fitness {} -> True
        _ -> False
    )
    bpm
hasFitness (Phenotype (These _ bpm)) =
  any
    ( \(PhenotypeData _ f) -> case f of
        Fitness {} -> True
        _ -> False
    )
    bpm
hasFitness _ = False

getFitness :: Phenotype -> Fitness
getFitness (Phenotype (This (PhenotypeData _ f@(Fitness {})))) = f
getFitness (Phenotype (This (PhenotypeData _ None))) = None
getFitness (Phenotype (That bmd)) = let f@(Fitness _ _ _) = foldr (<>) None $ fmap fitness bmd in f
getFitness (Phenotype (These _ bmd)) = let f@(Fitness _ _ _) = foldr (<>) None $ fmap fitness bmd in f

outputPhenotype :: Phenotype -> Text
outputPhenotype (Phenotype (This (PhenotypeData d _))) = d
outputPhenotype (Phenotype (These (PhenotypeData d _) _)) = d
