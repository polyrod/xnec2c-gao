{-# LANGUAGE OverloadedStrings #-}

module Display where

import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Float
import Text.Builder
import Types
import Utils

tab :: Builder
tab = char '\t'

nl :: Builder
nl = char '\n'

padr :: Builder -> Builder
padr = padFromRight 3 ' '

padl :: Builder -> Builder
padl = padFromLeft 11 ' '

toText :: Show a => a -> Builder
toText a = padl $ text $ T.replace "." "," $ T.pack $ show a

renderFitness :: Text -> Fitness -> Text
renderFitness _ None = ""
renderFitness lab (Fitness swr gain fbr) =
  run $
    (if T.null lab then mempty else tab <> padl (text lab))
      <> tab
      <> string "VSWR "
      <> padl (fixedDouble 4 $ float2Double swr)
      <> tab
      <> string "Raw Gain"
      <> padl (fixedDouble 2 $ float2Double gain)
      <> string " dBi"
      <> tab
      <> tab
      <> string "F/B Ratio "
      <> padl (fixedDouble 2 $ float2Double fbr)
      <> string " dB"

renderScore :: OptFun -> Phenotype -> Text
renderScore o pt =
  run $
    tab <> string "Score used for optimization (higher is better) : "
      <> tab
      <> padl (fixedDouble 2 $ float2Double $ score o pt)

renderOptModes :: GAO Text
renderOptModes = do
  s <- get
  pure $
    run $
      tab <> string "Optimizing for " <> padl (string (omodeShow (omode $ opts s))) <> nl
        <> tab
        <> string "Optimizing a   "
        <> padl (string (dmodeShow (dmode $ opts s)))
        <> " antenna."

printGenotype :: Individual -> IO ()
printGenotype i = do
  T.putStr $
    run $
      tab <> string "Genotype is <|"
        <> foldl (<>) mempty (fmap (\(k, v) -> text k <> ": " <> fixedDouble 4 (float2Double v) <> "|") $ M.assocs $ let Genotype g = genotype i in g)
        <> string ">"
        <> nl

printGeneration :: GAO ()
printGeneration = do
  s <- get
  liftIO $ T.putStrLn $ run $ string "Generation " <> decimal (genNum s) <> " of " <> decimal (genCount s)

printGenerationSummary :: [Individual] -> GAO ()
printGenerationSummary is = do
  s <- get
  let summary = run $
        mconcat $
          flip map (zip [(1 :: Int) ..] is) $ \(n, i) ->
            let p = fromJust . phenotype $ i
                (Fitness swr gain fbr) = getFitness p
                scr = score (optfun s) p
                line =
                  tab <> decimal n <> tab <> "Avg VSWR:  " <> padl (fixedDouble 2 (float2Double swr))
                    <> tab
                    <> "Raw Gain:  "
                    <> padl (fixedDouble 2 (float2Double gain))
                    <> " dBi"
                    <> tab
                    <> "F/B Ratio: "
                    <> padl (fixedDouble 2 (float2Double fbr))
                    <> " dB"
                    <> tab
                    <> "Score:     "
                    <> padl (fixedDouble 2 (float2Double scr))
                    <> nl
             in line
  liftIO $ T.putStrLn summary
