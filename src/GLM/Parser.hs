
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module GLM.Parser where

import qualified GLM.Tokenizer as T

import qualified Text.Parsec as P
import Text.Parsec ((<|>))
import Control.Lens
import Data.Either

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import Control.Applicative hiding (many, (<|>))

-- Data Types and Instances

data EntryItem = Prop   (String, String)
               | Nested Entry deriving (Show, Eq)

data Entry = Entry { _selector :: [String   ]
                   , _contents :: [EntryItem] } deriving (Show, Eq)

makePrisms ''EntryItem
makeLenses ''Entry

-- TODO: Temporary

type ParseResult = Either P.ParseError [Entry]

unSelector :: Entry -> [String]
unSelector = _selector
unContents :: Entry -> [EntryItem]
unContents = _contents

catProps :: Each s s EntryItem EntryItem => s -> [(String, String)]
catProps = toListOf (each . _Prop)

-- Tests

tests :: Test
tests = $(testGroupGenerator)

-- Positive Testing Properties

prop_topLevel_1, prop_topLevel_2, prop_topLevel_3, prop_topLevel_4, prop_topLevel_5, prop_topLevel_6, prop_topLevel_7 :: Bool
prop_topLevel_1 = isLen 1 $ glmParser "TEST" "x { y zzz; }"
prop_topLevel_2 = isLen 2 $ glmParser "TEST" "q { r s; }; t u {v w x;}"
prop_topLevel_3 = isLen 2 $ glmParser "TEST" "q { a {b c}; r s; }; t u {v w x;}"
prop_topLevel_4 = isLen 2 $ glmParser "TEST" "q { a {b c}; r s \"a super string!\"; }; t u {v w x;}"
prop_topLevel_5 = isLen 2 $ glmParser "TEST" "q { a {b c}; r s \"a super string!\"; }; t u {v w x;}\n"
prop_topLevel_6 = isLen 2 $ glmParser "TEST" "a { b c; }\n// comment\nc d { e f g; }"
prop_topLevel_7 = isLen 3 $ glmParser "TEST" "a { b c; }\nmodule tape;\nc d { e f g; }"

-- Imported from old parser

prop_glmParser_1, prop_glmParser_2, prop_glmParser_3, prop_glmParser_4 :: Bool
prop_glmParser_1 = isRight $ glmParser "TEST" "object foo {\n\thello world;\n}"
prop_glmParser_2 = isRight $ glmParser "TEST" "object foo {\n\thello world;\n}\n"
prop_glmParser_3 = isLen 2 $ glmParser "TEST" "object foo {\n\ta b;\n};\nobject bar {\n\tc d;\n};\n"
prop_glmParser_4 = isRight $ glmParser "TEST" "object foo { a b; object x { y z; }; }; object bar { c d; };"

-- Negative Testing Properties

prop_topLevel_neg_1 :: Bool
prop_topLevel_neg_1 = isLeft  $ glmParser "TEST" "a { b c;sdf''' \n\n/ }} }\n// comment\nc d { e f g; }"

-- Testing Helpers

isLen :: Int -> Either t [a] -> Bool
isLen n (Right l) = length l == n
isLen _ _ = False

-- String Parser combining Tokenization and GLM Parsing

glmParser :: FilePath -> String -> Either P.ParseError [Entry]
glmParser f s = do
  x <- P.parse T.parseTokens (f ++ " (TOKENS)") s
  y <- P.parse topLevel      (f ++ " (GLM)"   ) (stripComments x)
  return y

stripComments :: [(T.Token, b)] -> [(T.Token, b)]
stripComments = filter (not . (T.?> _1 . T._TComment))

-- Assume that comments have been stripped out of the stream

topLevel :: T.T [ Entry ]
topLevel = P.sepEndBy entry (P.optional T.pTSemi)

entry :: T.T Entry
entry = do
  sel <- P.many1 T.pTString
  braced sel <|> modl sel

modl :: [T.Token] -> T.T Entry
modl p = return $ Entry (selWords p) []

selWords :: Each s s T.Token T.Token => s -> [String]
selWords p = (p ^.. each . T._TString)

entryItems :: T.T [EntryItem]
entryItems = P.sepEndBy item T.pTSemi

item :: T.T EntryItem
item = do
  p <- prop
  P.try (nested p) <|> return (Prop (p ^. _head . T._TString, unwords $ p ^.. _tail . each . T._TString))

prop :: T.T [ T.Token ]
prop = P.many1 T.pTString

nested :: [T.Token] -> T.T EntryItem
nested p = Nested <$> braced p

braced :: [T.Token] -> T.T Entry
braced p = do
  T.pTLBrace
  c <- entryItems
  T.pTRBrace
  return $ Entry (selWords p) c
