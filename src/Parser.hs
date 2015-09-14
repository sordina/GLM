module Parser where

import Text.Parsec as P
import Control.Applicative hiding ((<|>))
import Data.Maybe
import Control.Monad
import Data.Either

type ParseResult = Either P.ParseError GLMFile
type P a         = Parsec String () a
data GLMFile     = GLMFile { unGLM :: [Entry] } deriving (Show)
type Property    = (String, String)
data Entry       = Entry   { unSelector :: [String]
                           , unContents :: [Property] }

instance Show Entry where
  show e = unwords (unSelector e)
        ++ "\n"
        ++ unlines (map ("\t" ++) (map show (unContents e)))

glmParser :: P GLMFile
glmParser = GLMFile <$> (P.skipMany comspa *> P.sepEndBy entry (P.many1 comspa))

prop_glmParser_1 = isRight $ P.parse glmParser "TEST" "object foo {\n\thello world;\n}"
prop_glmParser_2 = isRight $ P.parse glmParser "TEST" "object foo {\n\thello world;\n}\n"
prop_glmParser_3 = print   $ P.parse glmParser "TEST" "object foo {\n\ta b;\n};\nobject bar {\n\tc d;\n};\n"

comspa :: P ()
comspa = void comment <|> void (P.many1 P.space)

comment :: P String
comment = (P.string "#" <|> P.string "//") *> P.many (P.noneOf "\r\n") <* P.many P.endOfLine

entry :: P Entry
entry = do
  ws <- P.many1 $ P.noneOf "{"
  p  <- P.string "{" *> properties <* P.string "}" <* P.optional (P.string ";") -- P.try (P.many P.space <* P.string ";")
  return $ Entry (words ws) p

properties :: P [Property]
properties = catMaybes <$> P.sepEndBy property (P.string ";")

property :: P (Maybe Property)
property = do
  c <- P.many $ P.noneOf ";}" -- TODO
  let p = case words c of
            (h:t) -> Just (h, unwords t)
            []    -> Nothing
  return p

nestedEntry :: P (Maybe Property)
nestedEntry = return Nothing -- TODO

