{-# LANGUAGE OverloadedStrings #-}

module Display where

import Control.Concurrent
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.These
import GHC.Float
import TextBuilder
import TextBuilderDev
import Types
import Utils

tab :: TextBuilder
tab = char '\t'

nl :: TextBuilder
nl = char '\n'

padr :: TextBuilder -> TextBuilder
padr = padFromRight 3 ' '

padl :: TextBuilder -> TextBuilder
padl = padFromLeft 9 ' '

asText :: (Show a) => a -> TextBuilder
asText a = padl $ text $ T.replace "." "," $ T.pack $ show a

renderFitness :: Text -> Fitness -> Text
renderFitness _ None = ""
renderFitness lab (Fitness swr gain fbr) =
  toText $
    (if T.null lab then mempty else tab <> padl (text lab))
      <> tab
      <> string "VSWR "
      <> padl (doubleFixedPoint 4 $ float2Double swr)
      <> tab
      <> string "Raw Gain"
      <> padl (doubleFixedPoint 2 $ float2Double gain)
      <> string " dBi  "
      <> tab
      <> string "F/B Ratio "
      <> padl (doubleFixedPoint 2 $ float2Double fbr)
      <> string " dB"

renderScore :: OptFun -> Phenotype -> Text
renderScore o pt =
  toText $
    tab
      <> string "Score used for optimization (higher is better) : "
      <> tab
      <> padl (doubleFixedPoint 2 $ float2Double $ score o pt)

renderOptModes :: GAO Text
renderOptModes = do
  s <- get
  pure $
    toText $
      tab
        <> string "Optimizing for "
        <> string (omodeShow (omode $ opts s))
        <> nl
        <> tab
        <> string "Optimizing a "
        <> string (dmodeShow (dmode $ opts s))
        <> " antenna."

printGenotype :: Individual -> IO ()
printGenotype i = do
  T.putStr $
    toText $
      tab <> string "Genotype is " <> text (renderGenotype i) <> nl

renderGenotype :: Individual -> Text
renderGenotype i = do
  toText $
    tab
      <> string "<|"
      <> foldl (<>) mempty (fmap (\(k, v) -> text k <> ": " <> doubleFixedPoint 4 (float2Double v) <> "|") $ M.assocs $ let Genotype g = genotype i in g)
      <> string ">"

printGeneration :: GAO ()
printGeneration = do
  s <- get
  liftIO $ T.putStrLn $ toText $ string "Generation " <> decimal (genNum s) <> " of " <> decimal (genCount s)

printGenerationSummary :: [Individual] -> GAO ()
printGenerationSummary is = do
  s <- get
  let summary =
        toText $
          tab
            <> text "Generation "
            <> decimal (genNum s - 1)
            <> " selected survivors"
            <> nl
            <> tab
            <> text "========================================"
            <> nl
            <> nl
            <> mconcat
              ( flip map (zip [(1 :: Int) ..] is) $ \(n, i) ->
                  let p = fromJust . phenotype $ i
                      scr = score (optfun s) p
                      linetail None = mempty
                      linetail f@(Fitness _ gain fbr) =
                        tab
                          <> "Raw Gain:  "
                          <> padl (doubleFixedPoint 2 (float2Double gain))
                          <> " dBi  "
                          <> tab
                          <> "F/B Ratio: "
                          <> padl (doubleFixedPoint 2 (float2Double fbr))
                          <> " dB"
                          <> tab
                          <> "Score:     "
                          <> padl (doubleFixedPoint 2 (float2Double (let (OF h) = optfun s in h f)))
                          <> nl
                      entryhead = tab <> decimal n <> tab <> text "Genotype is " <> padl (text (renderGenotype i)) <> nl <> nl
                      entryfoot = nl <> tab <> tab <> "with Score " <> padl (doubleFixedPoint 2 (float2Double scr)) <> nl <> nl
                      entrybody = case getPhenotype p of
                        This (PhenotypeData _ f@(Fitness swr _ _)) ->
                          tab <> tab <> "AVSWR:  " <> padl (doubleFixedPoint 2 (float2Double swr)) <> linetail f
                        These _ bs ->
                          let ls = (\(Band lab _ _, PhenotypeData _ f@(Fitness swr _ _)) -> tab <> tab <> padl (text lab) <> tab <> "AVSWR: " <> padl (doubleFixedPoint 2 (float2Double swr)) <> linetail f) <$> M.assocs bs
                           in foldl (<>) mempty ls
                        _ -> mempty
                   in entryhead <> entrybody <> entryfoot
              )
  liftIO $ do
    T.putStrLn summary
    threadDelay 6000000
