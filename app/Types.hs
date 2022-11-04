{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Types where

import Control.Applicative
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Data.These

newtype Genotype a = Genotype {getGenotype :: Map Symbol a}
  deriving (Eq, Show, Functor, Foldable, Traversable)

emptyGenotype :: Genotype a
emptyGenotype = Genotype M.empty

data Fitness = None | Fitness Float Float Float
  deriving (Eq, Show)

instance Monoid Fitness where
  mempty = None

instance Semigroup Fitness where
  (<>) None None = None
  (<>) None a = a
  (<>) a None = a
  (<>) (Fitness a b c) (Fitness a' b' c') = Fitness (a + a') (b + b') (c + c')

data PhenotypeData = PhenotypeData {data_ :: Text, fitness :: Fitness}
  deriving (Eq, Show)

newtype Phenotype = Phenotype
  {getPhenotype :: These PhenotypeData (Map Band PhenotypeData)}
  deriving (Eq, Show)

type Gene = Float

type Range = (Float, Float)

data Individual = Individual
  { genotype :: Genotype Gene,
    phenotype :: Maybe Phenotype,
    environment :: Map Symbol Float
  }
  deriving (Eq, Show)

data OptimizingMode = VSWR | GAIN | VSWRGAIN
  deriving (Eq, Ord, Enum, Show, Read)

data DirectiveMode = SYMETRICAL | DIRECTIVE
  deriving (Eq, Ord, Enum, Show, Read)

data GAOOpts = GAOOpts
  { gaoFile :: String,
    verbosity :: Int,
    selectDistinct :: Bool,
    popSize :: Int,
    initGenCount :: Int,
    omode :: OptimizingMode,
    dmode :: DirectiveMode
  }
  deriving (Show)

defaultGAOOpts :: GAOOpts
defaultGAOOpts = GAOOpts "" 0 False 20 10 VSWRGAIN SYMETRICAL

newtype OptFun = OF {runOf :: Fitness -> Float}

instance Show OptFun where
  show = const "OptFun"

data GAOEnv = GAOEnv
  { done :: Bool,
    prototype :: Genotype Range,
    generation :: [Individual],
    genNum :: Int,
    genCount :: Int,
    opts :: GAOOpts,
    gaomodel :: GAOModel,
    bands :: [Band],
    xnec2c :: Maybe ThreadId,
    optfun :: OptFun
  }
  deriving (Show)

defaultGAOEnv :: GAOEnv
defaultGAOEnv = GAOEnv False emptyGenotype [] 1 0 defaultGAOOpts (GAOModel []) [] Nothing (OF (const 0))

newtype GAO a = GAO {runGAO :: GAOEnv -> IO (a, GAOEnv)}
  deriving
    (Functor, Applicative, Alternative, Monad, MonadIO, MonadPlus, MonadState GAOEnv)
    via StateT GAOEnv IO

data Expr
  = Lit Float
  | Var Text
  | BiOp BOp Expr Expr
  | UnOp UOp Expr
  deriving (Show)

data BOp = Add | Sub | Mult | Div | Exp
  deriving (Show)

data UOp = Negate | Sin | Cos | Sqrt
  deriving (Show)

newtype GAOModel = GAOModel (Deck Expr)
  deriving (Show)

type Deck a = [(Int, Card a)]

newtype Card a = Card (CardType a)
  deriving (Show)

data CardType a
  = CM Text
  | CE Text
  | GW CardTag SegmentCount (Point3 a) (Point3 a) (Radius a)
  | GE GroundType
  | FR Text
  | LD
  | GN
  | EX ExType
  | EK Kernel
  | RP
  | EN
  | SYM Symbol a
  | GSYM Symbol Range
  | GAOP
  | BND Band
  | Other Text Text
  deriving (Show)

data Band = Band
  { ident :: Text,
    width :: Range,
    steps :: Int
  }
  deriving (Eq, Ord, Show)

type Symbol = Text

type CardTag = Int

type SegmentCount = Int

newtype Radius a = Radius a
  deriving (Show)

data Point3 a = Point3 {x, y, z :: a}
  deriving (Show)

data GroundType = NoGround | GroundPlane | Ground
  deriving (Show)

data Kernel = TWK | ETWK
  deriving (Show)

data ExType = VS | IPWLP | IPWREP | IPWLEP | ECS | VSCSD
  deriving (Show)
