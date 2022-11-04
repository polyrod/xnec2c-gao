module Utils where

import Data.Text (Text)
import Data.These
import Types
import Text.Pretty.Simple
import Control.Monad.State

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

score :: Phenotype -> Float
score (Phenotype (This (PhenotypeData _ (Fitness vswr gain fbr)))) = calc vswr gain fbr
score (Phenotype (This (PhenotypeData _ None))) = 0
score (Phenotype (That bmd)) = case foldr (<>) None $ fmap fitness bmd of
  Fitness a b c -> calc a b c
  None -> 0
score (Phenotype (These _ bmd)) = case foldr (<>) None $ fmap fitness bmd of
  Fitness a b c -> calc a b c
  None -> 0


calc :: Fractional a => a -> a -> a -> a
calc a b c = 100 / a + 3 * b/100 + 1/(c*c)

ppState :: GAO ()
ppState = do
  s <- get
  pPrint s
