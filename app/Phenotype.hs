{-# LANGUAGE OverloadedStrings #-}

module Phenotype where

import Control.Monad.ListM
import Control.Monad.State
import Data.Angle
import qualified Data.ByteString.Char8 as B
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.These
import Display
import Output
import System.FilePath.Posix.ByteString
import System.Posix.Directory.ByteString
import Text.Builder
import Types
import Utils

type Env = Map Symbol Float

genPhenotypes :: GAO ()
genPhenotypes = do
  s <- get
  pts <- mapM geno2pheno $ generation s
  put $ s {generation = pts}

geno2pheno :: Individual -> GAO Individual
geno2pheno (Individual g Nothing env) = do
  s <- get
  let (GAOModel cards) = gaomodel s
  (env'', cards') <- mapAccumM (repack evalCard) env' cards :: GAO (Env, [(Int, Card Float)])
  d <- renderDeck (bands s) cards'
  return $ Individual g (Just d) env''
  where
    env' = let Genotype e = g in env `M.union` e
    repack :: (Env -> Card Expr -> GAO (Env, Card Float)) -> Env -> (Int, Card Expr) -> GAO (Env, (Int, Card Float))
    repack f e (i, ce) = do
      e' <- fst <$> f e ce
      cf <- snd <$> f e ce
      pure (e', (i, cf))
geno2pheno i = pure i

evalCard :: Env -> Card Expr -> GAO (Env, Card Float)
evalCard env (Card (SYM s e)) = do
  let v = eval env e
  return (extend env s v, Card (SYM s v))
evalCard env (Card (GW ct sc (Point3 spx spy spz) (Point3 epx epy epz) (Radius r))) = do
  let spx' = eval env spx
      spy' = eval env spy
      spz' = eval env spz
      epx' = eval env epx
      epy' = eval env epy
      epz' = eval env epz
      r' = eval env r
  return (env, Card (GW ct sc (Point3 spx' spy' spz') (Point3 epx' epy' epz') (Radius r')))
evalCard env (Card (CM t)) = pure (env, Card (CM t))
evalCard env (Card (CE t)) = pure (env, Card (CE t))
evalCard env (Card (EX t)) = pure (env, Card (EX t))
evalCard env (Card (EK t)) = pure (env, Card (EK t))
evalCard env (Card (FR t)) = pure (env, Card (FR t))
evalCard env (Card LD) = pure (env, Card LD)
evalCard env (Card GN) = pure (env, Card GN)
evalCard env (Card RP) = pure (env, Card RP)
evalCard env (Card (GE g)) = pure (env, Card (GE g))
evalCard env (Card EN) = pure (env, Card EN)
evalCard env (Card GAOP) = pure (env, Card GAOP)
evalCard env (Card (GSYM a b)) = pure (env, Card (GSYM a b))
evalCard env (Card (BND b)) = pure (env, Card (BND b))
evalCard env (Card (Other a b)) = pure (env, Card (Other a b))

extend :: Env -> Symbol -> Float -> Env
extend e s v = M.insert s v e

eval :: Env -> Expr -> Float
eval _ (Lit l) = l
eval env (Var v) = case M.lookup v env of
  Nothing -> error $ "Undefined Variable" ++ show v
  Just var -> var
eval env (BiOp Add e1 e2) = eval env e1 + eval env e2
eval env (BiOp Sub e1 e2) = eval env e1 - eval env e2
eval env (BiOp Mult e1 e2) = eval env e1 * eval env e2
eval env (BiOp Div e1 e2) = eval env e1 / eval env e2
eval env (BiOp Exp e1 e2) = eval env e1 ** eval env e2
eval env (UnOp Negate e) = negate $ eval env e
eval env (UnOp Sin e) = sine $ Degrees $ eval env e
eval env (UnOp Cos e) = cosine $ Degrees $ eval env e
eval env (UnOp Sqrt e) = sqrt $ eval env e

renderDeck :: [Band] -> Deck Float -> GAO Phenotype
renderDeck bs cs = do
  let p = PhenotypeData (T.concat (renderCard Nothing . snd <$> cs)) None
      ppb = M.fromList $ zip bs $ map (\b -> PhenotypeData (T.concat (renderCard (Just b) . snd <$> cs)) None) bs
  pure $
    if M.null ppb
      then Phenotype $ This p
      else Phenotype $ These p ppb

renderCard :: Maybe Band -> Card Float -> Text
renderCard _ (Card (CM t)) = run $ padr (string "CM") <> tab <> text t <> nl
renderCard _ (Card (CE t)) = run $ padr (string "CE") <> tab <> text t <> nl
renderCard _ (Card (GW ct sc (Point3 spx spy spz) (Point3 epx epy epz) (Radius r))) =
  run $
    padr (string "GW") <> tab <> padl (decimal ct) <> tab <> padl (decimal sc) <> tab
      <> toText spx
      <> tab
      <> toText spy
      <> tab
      <> toText spz
      <> tab
      <> toText epx
      <> tab
      <> toText epy
      <> tab
      <> toText epz
      <> tab
      <> toText r
      <> nl
renderCard _ (Card (SYM _ _)) = ""
renderCard _ (Card (GSYM _ _)) = ""
renderCard _ (Card (BND _)) = ""
renderCard Nothing (Card (FR t)) = run $ padr (string "FR") <> tab <> text (T.concat $ intersperse "\t" $ T.words t) <> nl
renderCard (Just _) (Card (FR _)) = ""
renderCard Nothing (Card EN) = run $ padr (string "EN") <> nl
renderCard (Just b) (Card EN) =
  let low = fst $ width b
      high = snd $ width b
      stps = steps b
      delta = (high - low) / fromIntegral stps
   in run $
        padr (string "FR") <> tab
          <> padl (decimal (0 :: Int))
          <> tab
          <> padl (decimal stps)
          <> tab
          <> decimal (0 :: Int)
          <> tab
          <> decimal (0 :: Int)
          <> tab
          <> toText low
          <> tab
          <> toText delta
          <> tab
          <> toText high
          <> nl
          <> padr (string "EN")
          <> nl
renderCard _ (Card (Other t1 t2)) = run $ padr (text t1) <> tab <> text (T.concat $ intersperse "\t" $ T.words t2) <> nl
renderCard _ _ = error "catchall"

evalPhenotypes :: GAO ()
evalPhenotypes = do
  s <- get

  printGeneration
  let ptc = Prelude.length $ generation s
  g' <-
    mapM
      ( \(idx, i) -> do
          liftIO $
            T.putStrLn $
              run $
                nl <> tab <> string "Phenotype "
                  <> decimal (idx :: Int)
                  <> string " of "
                  <> decimal ptc
                  <> nl
          i' <- runPhenotype i
          liftIO $ printGenotype i'
          liftIO $ T.putStr "\n"
          liftIO $ case getPhenotype $ fromJust $ phenotype i' of
            This (PhenotypeData _ f) -> T.putStrLn $ renderFitness "" f
            That bpm -> mapM_ (\(Band bi _ _, PhenotypeData _ f) -> T.putStrLn $ renderFitness bi f) $ M.assocs bpm
            These _ bpm -> mapM_ (\(Band bi _ _, PhenotypeData _ f) -> T.putStrLn $ renderFitness bi f) $ M.assocs bpm

          liftIO $ T.putStr "\n"
          liftIO $ T.putStrLn $ renderScore (optfun s) $ fromJust $ phenotype i'
          liftIO $ T.putStr "\n"
          modes <- renderOptModes
          liftIO $ T.putStrLn modes
          liftIO $ T.putStrLn "\n\n\n"

          return i'
      )
      $ zip [1 ..] $ generation s
  modify (\u -> u {generation = g', genNum = genNum u + 1, done = genNum u + 1 > genCount u})

runPhenotype :: Individual -> GAO Individual
runPhenotype i =
  do
    s <- get
    cwd <- liftIO getWorkingDirectory
    let necfile = cwd </> takeBaseName (B.pack $ gaoFile $ opts s) <.> "run.nec"
    startXnec2c necfile

    let pt@(Phenotype ps) = fromJust $ phenotype i
    if not $ hasFitness pt
      then case ps of
        This p -> liftIO $ do
          f <- runWithXnec necfile $ data_ p
          pure $ i {phenotype = Just $ Phenotype $ This $ PhenotypeData (data_ p) f}
        That bdm -> do
          brm <-
            liftIO $
              mapM
                ( \(b, PhenotypeData d _) -> do
                    f' <- runWithXnec necfile d
                    pure (b, PhenotypeData d f')
                )
                $ M.assocs bdm
          pure $ i {phenotype = Just $ Phenotype $ That $ M.fromList brm}
        These p bdm -> do
          brm <-
            liftIO $
              mapM
                ( \(b, PhenotypeData d _) -> do
                    f' <- runWithXnec necfile d
                    pure (b, PhenotypeData d f')
                )
                $ M.assocs bdm
          pure $ i {phenotype = Just $ Phenotype $ These p $ M.fromList brm}
      else pure i
