{-# LANGUAGE TypeApplications #-}

module Genetics where

import Control.Monad.State
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Display
import Genotype
import System.Random
import Types
import Utils

survivorRatio :: Double
survivorRatio = 0.3

selectSurvivors :: GAO ()
selectSurvivors = do
  s <- get
  let selector = if selectDistinct $ opts s then nub else id
      g = generation s
      gc = fromIntegral $ length g
      sc = floor $ gc * survivorRatio
      g' =
        take sc $
          sortBy
            ( flip
                ( \i i' ->
                    let getScore ind =
                          if score (optfun s) (fromJust (phenotype ind)) < 0
                            then 0
                            else score (optfun s) $ fromJust (phenotype ind)
                     in compare (getScore i) (getScore i')
                )
            )
            (selector g)
  --    dupmap = zipWith (\a _ -> floor $ fromIntegral (length g' - a) / (3.0 :: Double)) [1 ..] g'
  --    g'' = concat $ zipWith replicate dupmap g'
  -- printGenerationSummary g''
  -- modify (\u -> u {generation = g''})
  printGenerationSummary g'
  modify (\u -> u {generation = g'})

applyGenOperations :: GAO ()
applyGenOperations = do
  g <- gets generation
  g' <- concat <$> mapM genetics (nub g)
  modify (\s -> s {generation = nub (g <> g')})

-- TODO clamp,crossover
genetics :: Individual -> GAO [Individual]
genetics i = do
  r <- liftIO $ randomRIO (0.0, 1.0) :: GAO Float
  if r < 0.4
    then mutate i
    else crossover i

crossover :: Individual -> GAO [Individual]
crossover ind = do
  s <- get
  mateidx <- liftIO $ randomRIO (0, (length (generation s)) - 1)
  rcidx <- liftIO $ randomRIO (0, length (prototype s) - 1)
  let mate = generation s !! mateidx
      (ab, ba) = recombine rcidx ind mate
  pure $ nub [ab, ba]

recombine :: Int -> Individual -> Individual -> (Individual, Individual)
recombine i ind mate =
  let a = M.assocs $ let (Genotype g) = genotype ind in g
      b = M.assocs $ let (Genotype g) = genotype mate in g
      ha = take i a
      hb = take i b
      ta = drop i a
      tb = drop i b
      ab = M.fromList $ ha ++ tb
      ba = M.fromList $ hb ++ ta
   in (ind {genotype = Genotype ab}, mate {genotype = Genotype ba})

mutate :: Individual -> GAO [Individual]
mutate ind = do
  let Genotype gtm = genotype ind
  gtm' <-
    liftIO $
      mapM
        ( \v -> do
            mf <- randomRIO @Gene (-0.1, 0.1)
            af <- randomRIO @Gene (0, 1.0)
            pure $
              if af > 0.3
                then abs $ v + mf
                else v
        )
        gtm
  return $ pure $ ind {genotype = Genotype gtm', phenotype = Nothing, environment = M.empty}

genNextGen :: GAO ()
genNextGen = do
  s <- get
  let ps = popSize $ opts s
      gs = length $ generation s
      delta = ps - gs
  gts <-
    if delta > 0
      then (generation s <>) <$> generateNindividuals delta
      else pure $ take ps $ generation s
  modify (\u -> u {generation = gts})
