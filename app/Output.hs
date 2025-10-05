{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Output where

import Control.Concurrent
import Control.Monad
import Control.Monad.Loops
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import Data.IORef
import Data.List (nub)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory (removeFile)
import System.FilePath.Posix.ByteString
import System.INotify
import System.Posix.Directory.ByteString
import System.Process (spawnCommand, system, terminateProcess, waitForProcess)
import Types
import Utils

startXnec2c :: RawFilePath -> GAO ()
startXnec2c necfile = do
  s <- get
  unless (isJust $ xnec2c s) $ do
    let cmd = "xnec2c  --optimize -j7  -i " ++ B.unpack necfile ++ " > /dev/null 2>&1"
    xnec <- liftIO $ spawnCommand cmd
    modify (\u -> u {xnec2c = Just (XN xnec)})

runWithXnec :: RawFilePath -> Text -> IO (Maybe Fitness)
runWithXnec necfile bpheno = do
  csvReady <- liftIO $ newIORef False
  T.writeFile (B.unpack necfile) bpheno
  let touchcmd = "touch " ++ B.unpack necfile ++ ".csv"
  void $ system touchcmd
  withINotify $ \notify -> do
    d <- addWatch notify [Modify, Create] (necfile <.> "csv") (\_ -> writeIORef csvReady True)
    untilM_ (threadDelay 300000) (readIORef csvReady)
    threadDelay 300000
    removeWatch d
  csvData <- drop 1 . T.lines <$> T.readFile (decodeFilePath $ necfile <.> "csv")
  let linecount = fromIntegral $ Prelude.length csvData
  let bvswr = (1 / linecount) * sum ((\l -> read @Float $ T.unpack $ T.splitOn "," l !! 5) <$> csvData)
  let bgain = (1 / linecount) * sum ((\l -> read @Float $ T.unpack $ T.splitOn "," l !! 10) <$> csvData)
  let bfbr = (1 / linecount) * sum ((\l -> read @Float $ T.unpack $ T.splitOn "," l !! 16) <$> csvData)

  return $
    if (bvswr < 0) || (bvswr > 100)
      then Nothing
      else Just $ Fitness bvswr bgain bfbr

tidyUp :: GAO ()
tidyUp = do
  s <- get
  cwd <- liftIO getWorkingDirectory
  let basefile = gaoFile $ opts s
  let necfile = cwd </> takeBaseName (B.pack basefile) <.> ".run.nec"
  let csvfile = necfile <.> "csv"
  liftIO $ removeFile $ decodeFilePath necfile
  liftIO $ removeFile $ decodeFilePath csvfile
  liftIO $ do
    when (isJust $ xnec2c s) $
      do
        let (XN p) = fromJust $ xnec2c s
        terminateProcess p
        void $ waitForProcess p

outputResult :: GAO ()
outputResult = do
  s <- get
  let survivors = nub $ filter (\i -> isJust (phenotype i) && (hasFitness . fromJust . phenotype) i) $ generation s
  mapM_ toFile $ zip [1 ..] survivors

toFile :: (Int, Individual) -> GAO ()
toFile (n, i) = do
  s <- get
  let (Fitness vswr _ _) = getFitness $ fromJust $ phenotype i
      fn =
        gaoFile (opts s)
          ++ "_"
          ++ show n
          ++ "_[AVSWR:"
          ++ show vswr
          ++ "].nec"
      p = let t = fromJust $ phenotype i in outputPhenotype t
  liftIO $ T.writeFile fn p
