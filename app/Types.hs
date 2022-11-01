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
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Pretty.Simple

newtype Genotype a = Genotype {getGenotype :: Map Symbol a}
  deriving (Eq, Show, Functor, Foldable, Traversable)

emptyGenotype :: Genotype a
emptyGenotype = Genotype M.empty

newtype Phenotype = Phenotype {getPhenotype :: Text}
  deriving (Eq, Show)

type Gene = Float

type Range = (Float, Float)

data Individual = Individual
  { genotype :: Genotype Gene,
    phenotype :: Maybe Phenotype,
    env :: Map Symbol Float,
    score :: Maybe Float
  }
  deriving (Eq, Show)

data GAOOpts = GAOOpts
  { gaoFile :: String,
    verbosity :: Int,
    selectDistinct :: Bool,
    popSize :: Int,
    initGenCount :: Int
  }
  deriving (Show)

defaultGAOOpts = GAOOpts "" 0 False 20 10

data GAOEnv = GAOEnv
  { done :: Bool,
    prototype :: Genotype Range,
    generation :: [Individual],
    genNum :: Int,
    genCount :: Int,
    opts :: GAOOpts,
    gaomodel :: GAOModel,
    xnec2c :: Maybe ThreadId
  }
  deriving (Show)

defaultGAOEnv = GAOEnv False emptyGenotype [] 1 0 defaultGAOOpts (GAOModel []) Nothing

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
  | FR
  | LD
  | GN
  | EX ExType
  | EK Kernel
  | RP
  | EN
  | SYM Symbol a
  | GSYM Symbol Range
  | GAOP
  | Other Text Text
  deriving (Show)

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
