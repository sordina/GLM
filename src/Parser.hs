module Parser where

import Text.Parsec as P
import Control.Applicative hiding ((<|>))
import System.Environment
import Data.Maybe

type ParseResult = Either P.ParseError GLMFile
type P a         = Parsec String () a
data GLMFile     = GLMFile { unGLM :: [Entry] } deriving (Show)
type Property    = (String, String)
data Entry       = Entry   { unSelector :: [String]
                           , unContents :: [Property] }

instance Show Entry where
  show e = unwords (unSelector e)
        ++ "\n"
        ++ unlines (map ("\t"++) (map show (unContents e)))

glmParser :: P GLMFile
glmParser = GLMFile <$> (P.skipMany comspa *> P.sepBy entry (P.many1 comspa))

comspa :: P ()
comspa = comment <|> (P.many1 P.space *> return ())

comment :: P ()
comment = (P.string "#" <|> P.string "//") *> P.many (P.noneOf "\n") *> return ()

entry :: P Entry
entry = do
  ws <- selector
  o  <- openbrace
  p  <- properties
  c  <- closebrace
  return $ Entry (words ws) p

selector   = P.many $ P.noneOf "{"
openbrace  = P.string "{"
closebrace = P.string "}"

properties :: P [Property]
properties = catMaybes <$> P.sepBy (property <|> nestedEntry) (P.string ";")

property :: P (Maybe Property)
property   = do
  c <- P.many $ P.noneOf ";}"
  let p = case words c of
            (h:t) -> Just (h, unwords t)
            []    -> Nothing
  return p

nestedEntry :: P (Maybe Property)
nestedEntry = return Nothing -- TODO

