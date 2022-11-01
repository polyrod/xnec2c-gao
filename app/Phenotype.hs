{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Phenotype where

--import Utils

import Control.Concurrent
import Control.Monad.ListM
import Control.Monad.Loops
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import Data.IORef
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Lazy (toStrict)
import System.FilePath.Posix.ByteString
import System.INotify
import System.Posix.Directory.ByteString
import System.Process
import Text.Builder
import Text.Pretty.Simple
import Types

type Env = Map Symbol Float

genPhenotypes :: GAO ()
genPhenotypes = do
  s <- get
  pts <- mapM geno2pheno $ generation s
  put $ s {generation = pts}

--printState

geno2pheno :: Individual -> GAO Individual
geno2pheno (Individual g _ env Nothing) = do
  s <- get
  let (GAOModel cards) = gaomodel s
  (env'', cards') <- mapAccumM (repack evalCard) env' cards :: GAO (Env, [(Int, Card Float)])
  let d@(Phenotype x) = renderDeck cards'
  return $ Individual g (Just d) env'' Nothing
  where
    env' = let Genotype e = g in env `M.union` e
    repack :: (Env -> Card Expr -> GAO (Env, Card Float)) -> Env -> (Int, Card Expr) -> GAO (Env, (Int, Card Float))
    repack f e (i, ce) = do
      e' <- fst <$> f e ce
      cf <- snd <$> f e ce
      pure (e', (i, cf))
geno2pheno i@(Individual g _ env _) = pure i

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
evalCard env (Card FR) = pure (env, Card FR)
evalCard env (Card LD) = pure (env, Card LD)
evalCard env (Card GN) = pure (env, Card GN)
evalCard env (Card RP) = pure (env, Card RP)
evalCard env (Card EN) = pure (env, Card EN)
evalCard env (Card GAOP) = pure (env, Card GAOP)
evalCard env (Card (GSYM a b)) = pure (env, Card (GSYM a b))
evalCard env (Card (Other a b)) = pure (env, Card (Other a b))

extend :: Env -> Symbol -> Float -> Env
extend e s v = M.insert s v e

eval :: Env -> Expr -> Float
eval env (Lit x) = x
eval env (Var x) = case M.lookup x env of
  Nothing -> error $ "Undefined Variable" ++ show x
  Just v -> v
eval env (BiOp Add e1 e2) = eval env e1 + eval env e2
eval env (BiOp Sub e1 e2) = eval env e1 - eval env e2
eval env (BiOp Mult e1 e2) = eval env e1 * eval env e2
eval env (BiOp Div e1 e2) = eval env e1 / eval env e2
eval env (BiOp Exp e1 e2) = eval env e1 ** eval env e2
eval env (UnOp Negate e) = negate $ eval env e
eval env (UnOp Sin e) = sin $ eval env e
eval env (UnOp Cos e) = cos $ eval env e
eval env (UnOp Sqrt e) = sqrt $ eval env e

renderDeck cs = Phenotype (T.concat (renderCard . snd <$> cs))

tab = char '\t'

nl = char '\n'

renderCard :: Card Float -> Text
renderCard (Card (CM t)) = run $ padr (string "CM") <> tab <> text t <> nl
renderCard (Card (CE t)) = run $ padr (string "CE") <> tab <> text t <> nl
renderCard (Card (GW ct sc (Point3 spx spy spz) (Point3 epx epy epz) (Radius r))) =
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
renderCard (Card (Other t1 t2)) = run $ padr (text t1) <> tab <> text (T.concat $ intersperse "\t" $ T.words t2) <> nl
renderCard (Card (SYM _ _)) = ""
renderCard (Card (GSYM _ _)) = ""

padr = padFromRight 3 ' '

padl = padFromLeft 11 ' '

toText :: Show a => a -> Builder
toText a = padl $ text $ T.replace "." "," $ T.pack $ show a

evalPhenotypes :: GAO ()
evalPhenotypes = do
  s <- get
  printGeneration
  let ptc = Prelude.length $ generation s
  g' <-
    mapM
      ( \(i, p) -> do
          liftIO $
            putStrLn $
              "\nRunning Phenotype numeber "
                ++ show i
                ++ " of "
                ++ show ptc
          p' <- runPhenotype p
          liftIO $ putStrLn $ "AVG VSWR: " ++ show (fromJust $ score p')
          return p'
      )
      $ zip [1 ..] $ generation s
  modify (\s -> s {generation = g', genNum = genNum s + 1, done = genNum s + 1 > genCount s})

runPhenotype :: Individual -> GAO Individual
runPhenotype i@(Individual g _ env (Just _)) = do
  printGenotype i
  pure i
runPhenotype i = do
  s <- get
  csvReady <- liftIO $ newIORef False
  cwd <- liftIO getWorkingDirectory
  let necfile = cwd </> takeBaseName (B.pack $ gaoFile $ opts s) <.> "nec"

  liftIO $ T.writeFile (B.unpack necfile) $ let (Phenotype t) = fromJust $ phenotype i in t
  printGenotype i
  unless (isJust $ xnec2c s) $ do
    let cmd = "sleep 2 && xnec2c  --optimize -j2  -i " ++ B.unpack necfile ++ " > /dev/null 2>&1"
    --liftIO $ putStrLn cmd
    tid <- liftIO $ forkIO $ do
      void $ system cmd
    modify (\s -> s {xnec2c = Just tid})
  liftIO $ do
    let touchcmd = "touch " ++ B.unpack necfile ++ ".csv"
    void $ system touchcmd
    withINotify $ \notify -> do
      d <- addWatch notify [Modify, Create] (necfile <.> "csv") (\_ -> writeIORef csvReady True)
      untilM_ (threadDelay 300000) (readIORef csvReady)
      threadDelay 300000
      removeWatch d
  csvData <- liftIO $ drop 1 . T.lines <$> T.readFile (decodeFilePath $ necfile <.> "csv")
  let linecount = fromIntegral $ Prelude.length csvData
  let avgvswr = (1 / linecount) * sum ((\l -> read @Float $ T.unpack $ T.splitOn "," l !! 5) <$> csvData)

  return $ i {score = Just avgvswr}

printGenotype :: Individual -> GAO ()
printGenotype i = liftIO $ do
  putStr "<|"
  mapM_ (\(k, v) -> putStr $ T.unpack k ++ ": " ++ show v ++ "|") $ M.assocs $ let Genotype g = genotype i in g
  putStr ">\n"

printGeneration :: GAO ()
printGeneration = do
  s <- get
  liftIO $ putStrLn $ "Generation " ++ show (genNum s) ++ " of " ++ show (genCount s)
