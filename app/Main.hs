{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Text.IO as T
import GAOParser
import Genetics
import Genotype
import Options
import Phenotype
import System.IO
import System.Process (terminateProcess)
import Types
import Utils

main :: IO ()
main = do
  void $ runGAO optimizer defaultGAOEnv

optimizer :: GAO ()
optimizer = do
  parseOptions
  parseGAOFile
  genInitGenotypes
  goGAO
  outputResult

goGAO :: GAO ()
goGAO = do
  genPhenotypes
  evalPhenotypes
  selectSurvivors
  applyGenOperations
  genNextGen
  rerun

rerun :: GAO ()
rerun = do
  s <- get
  if done s
    then askProceed
    else goGAO

askProceed :: GAO ()
askProceed = do
  s <- get
  a <- liftIO $ do
    hSetBuffering stdin NoBuffering
    putStrLn $ "Are you satisfied and want to [Q]uit or [P]roceed for another " ++ show (initGenCount $ opts s) ++ " generations ?"
    a <- liftIO getChar
    putStr "\n"
    hSetBuffering stdin LineBuffering
    return a
  case toLower a of
    'q' -> pure ()
    'p' -> modify (\u -> u {genCount = genCount u + initGenCount (opts u)}) >> goGAO
    _ -> askProceed

outputResult :: GAO ()
outputResult = do
  s <- get
  let survivors = nub $ filter (\i -> isJust (phenotype i) && (hasFitness . fromJust . phenotype) i) $ generation s
  mapM_ toFile $ zip [1 ..] survivors
  liftIO $ terminateProcess $ let (XN p) = fromJust $ xnec2c s in p

toFile :: (Int, Individual) -> GAO ()
toFile (n, i) = do
  s <- get
  let (Fitness vswr _ _) = getFitness $ fromJust $ phenotype i
      fn =
        gaoFile (opts s) ++ "_" ++ show n
          ++ "_[AVSVR:"
          ++ show vswr
          ++ "].nec"
      p = let t = fromJust $ phenotype i in outputPhenotype t
  liftIO $ T.writeFile fn p
