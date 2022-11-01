{-# LANGUAGE TypeApplications #-}

module Genetics where

import Control.Monad.State
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Genotype
import System.Random
import Types

selectSurvivors :: GAO ()
selectSurvivors = do
  s <- get
  let selector = if selectDistinct $ opts s then nub else id
      g = generation s
      gc = fromIntegral $ length g
      sc = floor $ gc * 0.6
      g' =
        take sc $
          sortBy
            ( \i i' ->
                let getScore x =
                      if fromJust (score x) < 0
                        then 100000
                        else fromJust $ score x
                 in compare (getScore i) (getScore i')
            )
            $ selector g
      dupmap = zipWith (\a b -> floor $ fromIntegral (length g' - a) / 5.0) [1 ..] g'
      g'' = concat $ zipWith replicate dupmap g'
  modify (\s -> s {generation = g''})
  liftIO $ mapM (print . score) g''
  pure ()

applyGenOperations :: GAO ()
applyGenOperations = do
  g <- gets generation
  g' <- mapM genetics g
  modify (\s -> s {generation = g <> g'})

-- TODO clamp,crossover
genetics :: Individual -> GAO Individual
genetics i = do
  g <- gets generation
  mutate i
  where
    crossover = id
    mutate i = do
      let Genotype gtm = genotype i
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
      return $ i {genotype = Genotype gtm', phenotype = Nothing, env = M.empty, score = Nothing}

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
  modify (\s -> s {generation = gts})
