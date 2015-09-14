
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Text.Parsec as P
import Data.Maybe
import System.Environment
import Parser
import Data.Digest.Pure.MD5
import Data.List
import Data.String.Interpolate
import qualified Data.ByteString.Lazy.Char8 as BS

data Options = Options { edges :: Bool } deriving (Eq, Show)

withEdges = Options { edges = True  }
noEdges   = Options { edges = False }

str5 :: String -> String
str5 = take 9 . show . md5 . BS.pack

main :: IO ()
main = getArgs >>= start

start :: [String] -> IO ()
start ["-h"     ] = help
start ["--help" ] = help
start ("-e"     : args) = go args >>= mapM_ (outputResult withEdges)
start ("--edges": args) = go args >>= mapM_ (outputResult withEdges)
start args              = go args >>= mapM_ (outputResult noEdges)

outputResult :: Show a => Options -> Either a GLMFile -> IO ()
outputResult _    (Left  issue)   = print "Got an error:" >> print issue
outputResult opts (Right results) = putStrLn [i|digraph {#{unl $ concatMap graph (filter crit ung)}}|]
  where
  unl s = "\n" ++ unlines (map ("\t" ++) s)
  ung   = unGLM results
  rt    = concatMap refs ung
  crit  = criteria (edges opts) rt

criteria False _ _ = True
criteria True  l e = isJust $ find (== name e) l

refs :: Entry -> [String]
refs e@(Entry _ c) = fromMaybe [] $ do
  f <- lookup "from" c
  t <- lookup "to"   c
  return [name e, f, t]

help :: IO ()
help = putStrLn "Usage: glm2dot [FILE]*"

go :: [String] -> IO [ParseResult]
go xs@(_:_) = mapM processFile xs
go []       = (return . processContents "<STDIN>") `fmap` getContents

processFile :: String -> IO ParseResult
processFile f = processContents f `fmap` readFile f

processContents :: String -> String -> ParseResult
processContents = P.parse glmParser

graph :: Entry -> [String]
graph e@(Entry ("object":s:_) c) = [i|"#{nhash e}" [label="#{name e}"];|] : fromMaybe [] (edge c)
graph e@(Entry s c) = [ [i|// Missed entry #{s} - #{name e}|] ]

edge c = do
  f <- lookup "from" c
  t <- lookup "to"   c
  return [[i|"#{str5 f}" -> "#{str5 t}"; // #{f} -> #{t}|]]

nhash :: Entry -> String
nhash = str5 . name

name :: Entry -> String
name (Entry (_:s:_) c) = maybe s noquote (lookup "name" c)
name _                 = "noname"

noquote = filter (/= '\'')
