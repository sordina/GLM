{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Parser where

import Text.Parsec as P
import Control.Applicative hiding ((<|>))
import Data.Maybe
import Data.String
import Control.Monad
import Data.Either

-- Data Types

type ParseResult = Either P.ParseError [Entry]
type P a         = Parsec String () a
type Property    = (String, String)
data Entry       = Entry   { unSelector :: [String]
                           , unContents :: [Item  ] }

data Item = Prop Property | Nested Entry deriving Show

fromProp (Prop x) = Just x
fromProp _        = Nothing

catProps = mapMaybe fromProp

instance Show Entry where
  show e = unwords (unSelector e)
        ++ "\n"
        ++ unlines (map (("\t" ++) . show) (unContents e))

instance s ~ String => IsString (P s) where
  fromString = P.string

-- Parsers

glmParser :: P [Entry]
glmParser = P.skipMany comment_or_space *> P.sepEndBy entry (P.many1 comment_or_space)

comment_or_space :: P ()
comment_or_space = void comment <|> void (P.many1 P.space)

comment :: P String
comment = ("#" <|> "//") *> P.many (P.noneOf "\r\n") <* P.many P.endOfLine

entry :: P Entry
entry = do
  ws <- P.many1 $ P.noneOf "{"
  p  <- "{" *> P.sepEndBy item comment_or_space <* "}" <* P.optional ";"
  return $ Entry (words ws) (catMaybes p)

item :: P (Maybe Item)
item = P.try (property <* ";") <|> P.try nestedEntry

property :: P (Maybe Item)
property = do
  c <- P.many $ P.noneOf ";}{" -- TODO
  let p = case words c of
            (h:t) -> Just (Prop (h, unwords t))
            []    -> Nothing
  return p

nestedEntry :: P (Maybe Item)
nestedEntry = (Just . Nested) <$> entry


-- Testing Properties

prop_glmParser_1 = isRight   $ P.parse glmParser "TEST" "object foo {\n\thello world;\n}"
prop_glmParser_2 = isRight   $ P.parse glmParser "TEST" "object foo {\n\thello world;\n}\n"
prop_glmParser_3 = objects 2 $ P.parse glmParser "TEST" "object foo {\n\ta b;\n};\nobject bar {\n\tc d;\n};\n"
prop_glmParser_4 = isRight   $ P.parse glmParser "TEST" "object foo { a b; object x { y z; }; }; object bar { c d; };"

objects n (Right x) = length x == n
objects n _         = False
