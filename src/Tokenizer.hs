{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tokenizer where

import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import Data.String
import Data.Either
import Data.Maybe
import Data.Monoid
import Control.Lens
import Control.Applicative hiding (many, (<|>))

-- Data Types and Instances

type P a = P.Parsec String     () a
type T a = P.Parsec [TokenPos] () a

type TokenPos = (Token, P.SourcePos)

data Token = TString  String
           | TComment String
           | LBrace
           | RBrace
           | Semi
           deriving (Eq, Show)

makePrisms ''Token

instance s ~ String => IsString (P s) where fromString = P.string
instance t ~ Token  => IsString (T t) where fromString = pTParse -- Should just "Do the right thing"

(?>) :: s -> Getting (First a) s a -> Bool
d ?> p = isJust $ d ^? p

parseTokens :: P [TokenPos]
parseTokens = P.sepEndBy parseTokenPos (P.many P.space)

parseTokenPos :: P TokenPos
parseTokenPos = do
  pos <- P.getPosition
  tok <- pToken
  return (tok, pos)

pToken :: P Token
pToken = pComment <|> pLBrace <|> pRBrace <|> pSemi <|> pString <|> pWord

prop_tokens_1, prop_tokens_2, prop_tokens_3, prop_tokens_4, prop_tokens_5 :: Bool
prop_tokens_1 = isRight $ P.parse parseTokens "TEST" "\"he\\\"as\\ndf\\\"llo\";;;"
prop_tokens_2 = isRight $ P.parse parseTokens "TEST" "\"he\\\"as\\ndf\\\"llo\";  ;;\n//wtf  alksdjhfs \n;;"
prop_tokens_3 = Right LBrace == P.parse pToken "TEST" "{"
prop_tokens_4 = Right RBrace == P.parse pToken "TEST" "}"
prop_tokens_5 = Right RBrace == P.parse pToken "TEST" "}"

-- TokenPos Combinators

pTStringE, pTCommentE, pTParse :: String -> T Token
pTParse    s = tSatisfy ((== P.parse pToken "inline" s) . Right . fst)
pTStringE  s = tSatisfy ((== TString  s) . fst)
pTCommentE s = tSatisfy ((== TComment s) . fst)

pTLBrace, pTRBrace, pTSemi, pTComment, pTString, pTAny :: T Token
pTAny     = tSatisfy (const True)
pTString  = tSatisfy (?> _1 . _TString )
pTComment = tSatisfy (?> _1 . _TComment)
pTLBrace  = tSatisfy (?> _1 . _LBrace  )
pTRBrace  = tSatisfy (?> _1 . _RBrace  )
pTSemi    = tSatisfy (?> _1 . _Semi    )

-- Token Parsers

pString, pWord, pLBrace, pRBrace, pSemi, pComment :: P Token
pString  = fmap TString parseString
pWord    = fmap TString parseWord
pLBrace  = "{" *> return LBrace
pRBrace  = "}" *> return RBrace
pSemi    = ";" *> return Semi
pComment = do
  start <- "#" <|> "//"
  body  <- P.many $ P.noneOf "\r\n"
  P.optional P.endOfLine
  return $ TComment $ start ++ body

-- Word Parser

parseWord :: P String
parseWord = P.many1 (P.noneOf " ;\n\t\r{}\"\\/#")

-- String Parser

escape :: P String
escape = do
    d <- P.char '\\'
    c <- P.oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
    return [d, c]

nonEscape :: P Char
nonEscape = P.noneOf "\\\"\0\n\r\v\t\b\f"

character :: P String
character = fmap return nonEscape <|> escape

parseString :: P String
parseString = fmap concat ( P.char '"' *> P.many character <* P.char '"' )

-- Primitives for token stream

advance :: P.SourcePos -> t -> [TokenPos] -> P.SourcePos
advance _ _ ((_, pos) : _) = pos
advance pos _ [] = pos

tSatisfy :: (TokenPos -> Bool) -> T Token
tSatisfy f = P.tokenPrim show advance (\c -> if f c then Just (fst c) else Nothing)
