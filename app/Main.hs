{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text.IO as T
import System.INotify
import System.IO
import Text.Pretty.Simple

import Types
import Utils
import Options
import GAOParser
import Genotype
import Phenotype
import Genetics

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
    'p' -> modify (\s -> s {genCount = genCount s + initGenCount (opts s)}) >> goGAO
    _ -> askProceed

outputResult = do
  s <- get
  let survivors = nub $ filter (isJust . score) $ generation s
  mapM_ toFile $ zip [1 ..] survivors
  liftIO $ killThread $ fromJust $ xnec2c s
  pPrint s

toFile :: (Int, Individual) -> GAO ()
toFile (n, i) = do
  s <- get
  let fn =
        gaoFile (opts s) ++ "_" ++ show n
          ++ "_[AVSVR:"
          ++ show (fromJust $ score i)
          ++ "].nec"
      p = let Phenotype t = fromJust $ phenotype i in t
  liftIO $ T.writeFile fn p
