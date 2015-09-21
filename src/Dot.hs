
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Maybe
import System.Environment
import System.Exit
import Parser2
import Data.Digest.Pure.MD5
import Data.List
import Data.String.Interpolate
import qualified Data.ByteString.Lazy.Char8 as BS

data Options = Options { edges :: Bool } deriving (Eq, Show)

withEdges, noEdges :: Options
withEdges = Options { edges = True  }
noEdges   = Options { edges = False }

str5 :: String -> String
str5 = take 9 . show . md5 . BS.pack

main :: IO ()
main = getArgs >>= start

start :: [String] -> IO ()
start ["-h"     ]       = help
start ["--help" ]       = help
start ("-e"     : args) = go args >>= mapM_ (outputResult withEdges)
start ("--edges": args) = go args >>= mapM_ (outputResult withEdges)
start args              = go args >>= mapM_ (outputResult noEdges)

outputResult :: Options -> ParseResult -> IO ()
outputResult _    (Left  issue)   = putStrLn "Got an error:" >> print issue >> exitFailure
outputResult opts (Right results) = putStrLn [i|digraph {#{unl $ concatMap graph (filter crit ung)}}|]
  where
  unl s = "\n" ++ unlines (map ("\t" ++) s)
  ung   = results
  rt    = concatMap refs ung
  crit  = criteria (edges opts) rt

criteria :: Bool -> [String] -> Entry -> Bool
criteria False _ _ = True
criteria True  l e = isJust $ find (== name e) l

refs :: Entry -> [String]
refs e@(Entry _ p) = fromMaybe [] $ do
  f <- lookup "from" c
  t <- lookup "to"   c
  return [name e, f, t]
  where
  c = catProps p

help :: IO ()
help = putStrLn "Usage: glm2dot [-h] [-e] [FILE]*"

go :: [String] -> IO [ParseResult]
go xs@(_:_) = mapM processFile xs
go []       = (return . glmParser "<STDIN>") `fmap` getContents

processFile :: String -> IO ParseResult
processFile f = glmParser f `fmap` readFile f

chash :: Entry -> String
chash = (++ "ef") . take 4 . str5 . (!! 1) . unSelector

graph :: Entry -> [String]
graph e@(Entry ("object":_:_) _) = fromMaybe [ [i|"#{nhash e}" [label="#{name e}", fillcolor="##{chash e}", style=filled];|] ] (edge e)
graph e@(Entry s _) = [ [i|// Missed entry #{s} - #{name e}|] ]

edge :: Entry -> Maybe [String]
edge e@(Entry _ p) = do
  f <- lookup "from" c
  t <- lookup "to"   c
  return [[i|"#{str5 f}" -> "#{str5 t}" [label="#{name e}"]; // #{f} -> #{t}|]]
  where
  c = catProps p

nhash :: Entry -> String
nhash = str5 . name

name :: Entry -> String
name (Entry (_:s:_) p) = maybe s noquote (lookup "name" c)
  where
  c = catProps p
name _                 = "noname"

-- TODO: Shouldn't need this now...
--
noquote :: String -> String
noquote = filter (/= '\'')
