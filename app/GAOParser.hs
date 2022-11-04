{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module GAOParser where

import Control.Monad.Combinators.Expr
import Control.Monad.IO.Class
import Control.Monad.State
import qualified Data.Char as Char
import Data.List (foldl')
import Data.Maybe
import Data.Proxy
import Data.Scientific (Scientific, toRealFloat)
import qualified Data.Scientific as Sci
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug
import Text.Pretty.Simple
import Types

type Parser = ParsecT Void Text GAO

sc :: Parser ()
sc = L.space hspace1 empty empty

lexeme = L.lexeme sc

symbol = L.symbol sc

lineEnd :: Parser ()
--lineEnd = try ((newline *> pure ()) <|>  (eof >> pure ()))
lineEnd = gaodbg "NL" $ recover $ choice [void (single '\n'), eof, void newline, void crlf, void eol]
  where recover = withRecovery $ \e -> do
                    registerParseError e
                    some (anySingleBut '\n')
                    void newline


signedNum :: Parser Float
--signedNum = lexeme $ toRealFloat <$> L.signed sc L.scientific
signedNum = lexeme $ toRealFloat <$> L.signed sc cscientific

gaoparser :: Parser GAOModel
gaoparser = do
  GAOModel <$> deck

deck = do
  zip [1 ..] <$> (card `sepEndBy` lineEnd)

card = do
  choice
    [ cmCard,
      ceCard,
      symCard,
      gsymCard,
      bndCard,
      gwCard,
      enCard,
      geCard,
      frCard,
      ldCard,
      gnCard,
      exCard,
      ekCard,
      rpCard,
      symCard,
      gsymCard,
      gaoCard,
      otherCard
    ]

cmCard,
  ceCard,
  symCard,
  gsymCard,
  bndCard,
  gwCard,
  geCard,
  frCard,
  ldCard,
  gnCard,
  exCard,
  ekCard,
  rpCard,
  gaoCard,
  otherCard ::
    Parser (Card Expr)
cmCard = gaodbg "CM" $ do
  lexeme $ symbol "CM"
  t <- some printChar
  return $ Card $ CM $ T.pack t
ceCard = gaodbg "CE" $ do
  lexeme $ symbol "CE"
  t <- some printChar
  return $ Card $ CE $ T.pack t
symCard = gaodbg "symCard" $ do
  lexeme $ symbol "SYM"
  i <- identifier
  lexeme $ string ":="
  e <- lexeme $ gaodbg "Expr" expr
  return $ Card $ SYM i e
gsymCard = gaodbg "gsymCard" $ do
  lexeme $ symbol "GSYM"
  i <- gaodbg "Ident" identifier
  lexeme $ string ":="
  r <- lexeme $ gaodbg "Rnge" range
  return $ Card $ GSYM i r
gaoCard = otherCard
gwCard = do
  lexeme $ symbol "GW"
  ct <- lexeme L.decimal
  sc <- lexeme L.decimal
  [spx, spy, spz] <- count 3 $ lexeme expr
  [epx, epy, epz] <- count 3 $ lexeme expr
  r <- lexeme expr
  return $ Card $ GW ct sc (Point3 spx spy spz) (Point3 epx epy epz) (Radius r)
bndCard = gaodbg "bndCard" $ do
  lexeme $ symbol "BND"
  i <- T.pack <$> lexeme (some alphaNumChar)
  w <- do
    l <- toRealFloat <$> lexeme cscientific
    h <- toRealFloat <$> lexeme cscientific
    pure (l,h)
  Card . BND . Band i w  <$> L.decimal 

geCard = otherCard
frCard = gaodbg "FR" $ do
  lexeme $ symbol "FR"
  t <- some printChar
  return $ Card $ FR $ T.pack t
ldCard = otherCard
gnCard = otherCard
exCard = otherCard
ekCard = otherCard
rpCard = otherCard

enCard = gaodbg "enCard" $ do
  lexeme $ symbol "EN"
  _ <- lexeme $ many printChar
  return $ Card $ EN


otherCard = gaodbg "Other" $ do
  ct <- lexeme $ some alphaNumChar
  rest <- lexeme $ many printChar
  return $ Card $ Other (T.pack ct) (T.pack rest)

identifier :: Parser Text
identifier = do
  t <- lexeme ((:) <$> letterChar <*> many (choice [alphaNumChar,single '_']))
  return $ T.pack t

range :: Parser Range
range = do
  lexeme $
    between "[" "]" $ do
      l <- signedNum
      lexeme $ string "..."
      u <- signedNum
      return (l, u)

literal = Lit <$> lexeme signedNum

variable = Var <$> identifier
parens p = between (symbol "(") (symbol ")") $ lexeme p

term :: Parser Expr
term = do
  choice
    [ parens expr,
      variable,
      literal
    ]

expr = makeExprParser term optable

optable =
  [ [ prefix "SIN" (UnOp Sin),
      prefix "COS" (UnOp Cos),
      prefix "SQRT" (UnOp Sqrt),
      prefix "-" (UnOp Negate)
    ],
    [ binary "^" (BiOp Exp)],
    [ binary "*" (BiOp Mult),
      binary "/" (BiOp Div)
    ],
    [ binary "+" (BiOp Add),
      binary "-" (BiOp Sub)
    ]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary n f = InfixL (f <$ symbol n)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix n f = Prefix (f <$ symbol n)
postfix n f = Postfix (f <$ symbol n)

parseGAOFile :: GAO ()
parseGAOFile = do
  s <- get
  let fn = gaoFile $ opts s
  fc <- liftIO $ T.readFile fn
  pr <- runParserT gaoparser fn fc
  case pr of
    Left bundle -> liftIO $ do
      putStr (errorBundlePretty bundle)
      errorWithoutStackTrace "Abort"
    Right m -> do
      put s {gaomodel = m}
      when (verbosity (opts s) > 0) $ liftIO $ pPrint m

gaodbg :: Show a => String -> Parser a -> Parser a
gaodbg str p = do
  s <- lift get
  if verbosity (opts s) > 1
    then dbg str p
    else p
--
-- @since 5.0.0
cscientific ::
  forall e s m.
  (MonadParsec e s m, Token s ~ Char) =>
  m Scientific
cscientific = do
  c' <- decimal_
  SP c e' <- option (SP c' 0) (try $ commaDecimal_ (Proxy :: Proxy s) c')
  e <- option e' (try $ exponent_ e')
  return (Sci.scientific c e)
{-# INLINEABLE cscientific #-}

data SP = SP !Integer {-# UNPACK #-} !Int

commaDecimal_ ::
  (MonadParsec e s m, Token s ~ Char) =>
  Proxy s ->
  Integer ->
  m SP
commaDecimal_ pxy c' = do
  void (C.char ',')
  let mkNum = foldl' step (SP c' 0) . chunkToTokens pxy
      step (SP a e') c =
        SP
          (a * 10 + fromIntegral (Char.digitToInt c))
          (e' - 1)
  mkNum <$> takeWhile1P (Just "digit") Char.isDigit
{-# INLINE commaDecimal_ #-}

exponent_ ::
  (MonadParsec e s m, Token s ~ Char) =>
  Int ->
  m Int
exponent_ e' = do
  void (C.char' 'e')
  (+ e') <$> L.signed (return ()) decimal_
{-# INLINE exponent_ #-}

-- | A non-public helper to parse decimal integers.
decimal_ ::
  forall e s m a.
  (MonadParsec e s m, Token s ~ Char, Num a) =>
  m a
decimal_ = mkNum <$> takeWhile1P (Just "digit") Char.isDigit
  where
    mkNum = foldl' step 0 . chunkToTokens (Proxy :: Proxy s)
    step a c = a * 10 + fromIntegral (Char.digitToInt c)
{-# INLINE decimal_ #-}
