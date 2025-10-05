module Utils where

import Control.Monad.State
import qualified Data.Map as M
import Data.Text (Text)
import Data.These
import Text.Pretty.Simple
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
getFitness (Phenotype (That bmd)) =
  let (Fitness s g fb) = foldr (<>) None $ fmap fitness bmd
      l = fromIntegral $ M.size bmd
   in Fitness (s / l) (g / l) (fb / l)
getFitness (Phenotype (These _ bmd)) = getFitness (Phenotype (That bmd))

outputPhenotype :: Phenotype -> Text
outputPhenotype (Phenotype (This (PhenotypeData d _))) = d
outputPhenotype (Phenotype (These (PhenotypeData d _) _)) = d
outputPhenotype (Phenotype (That _)) = error "We won't output BNDs"

score :: OptFun -> Phenotype -> Float
score (OF h) (Phenotype (This (PhenotypeData _ f@(Fitness {})))) = h f
score _ (Phenotype (This (PhenotypeData _ None))) = 0
score (OF h) p = case getFitness p of
  f@(Fitness {}) -> h f
  None -> 0

omodeShow :: OptimizingMode -> String
omodeShow VSWR = "vswr"
omodeShow GAIN = "gain"
omodeShow VSWRGAIN = "vswr+gain"

dmodeShow :: DirectiveMode -> String
dmodeShow SYMMETRICAL = "symmetrical"
dmodeShow DIRECTIVE = "directive"

ppState :: GAO ()
ppState = do
  s <- get
  pPrint s
