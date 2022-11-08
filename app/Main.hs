{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Char
import GAOParser
import Genetics
import Genotype
import Options
import Output
import Phenotype
import System.IO
import Types

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
  tidyUp

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
