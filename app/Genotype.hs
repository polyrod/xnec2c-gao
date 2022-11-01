module Genotype where

import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe (fromJust)
import System.Random
import Types

genInitGenotypes = do
  proto <- extractPrototypeFromModel
  modify (\s -> s {prototype = proto})
  s <- get
  let ps = popSize $ opts s
  gts <- generateNindividuals ps
  modify (\s -> s {generation = gts})

generateNindividuals :: Int -> GAO [Individual]
generateNindividuals n = do
  s <- get
  fmap (\gt -> Individual gt Nothing M.empty Nothing) <$> replicateM n proto2geno

extractPrototypeFromModel :: GAO (Genotype Range)
extractPrototypeFromModel = do
  s <- get
  let (GAOModel cs) = gaomodel s
  pure $
    Genotype $
      M.fromList $
        (\(i, Card (GSYM s r)) -> (s, r))
          <$> filter
            ( \(i, Card ct) -> case ct of
                GSYM s r -> True
                _ -> False
            )
            cs

proto2geno :: GAO (Genotype Gene)
proto2geno = do
  s <- get
  mapM range2gene $ prototype s
  where
    range2gene = randomRIO
