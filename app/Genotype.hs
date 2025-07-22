module Genotype where

import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import System.Random
import Types

genInitGenotypes :: GAO ()
genInitGenotypes = do
  proto <- extractPrototypeFromModel
  bs <- extractBandsFromModel
  modify (\s -> s {prototype = proto, bands = bs})
  s <- get
  let ps = popSize $ opts s
  gts <- generateNindividuals ps
  modify (\e -> e {generation = gts})

generateNindividuals :: Int -> GAO [Individual]
generateNindividuals n = do
  fmap (\gt -> Individual gt Nothing M.empty) <$> replicateM n proto2geno

extractPrototypeFromModel :: GAO (Genotype Range)
extractPrototypeFromModel = do
  s <- get
  let (GAOModel cs) = gaomodel s
  pure $
    Genotype $
      M.fromList $
        (\(_, Card (GSYM n r)) -> (n, r))
          <$> filter
            ( \(_, Card ct) -> case ct of
                GSYM _ _ -> True
                _ -> False
            )
            cs

extractBandsFromModel :: GAO [Band]
extractBandsFromModel = do
  s <- get
  let (GAOModel cs) = gaomodel s
  pure $ [b | c <- filter (\(Card ct) -> case ct of BND _ -> True; _ -> False) $ map snd cs, let (Card (BND b)) = c]

proto2geno :: GAO (Genotype Gene)
proto2geno = do
  s <- get
  mapM range2gene $ prototype s
  where
    range2gene = randomRIO
