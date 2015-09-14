
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Text.Parsec as P
import Data.Maybe
import System.Environment
import Parser
import Data.Digest.Pure.MD5
import Data.String.Interpolate
import qualified Data.ByteString.Lazy.Char8 as BS

str5 :: String -> String
str5 = take 9 . show . md5 . BS.pack

main :: IO ()
main = getArgs >>= start

start :: [String] -> IO ()
start ["-h"]     = help
start ["--help"] = help
start args       = go args >>= mapM_ outputResult

outputResult :: Show a => Either a GLMFile -> IO ()
outputResult (Left  issue)   = print "Got an error:" >> print issue
outputResult (Right results) = putStrLn [i|digraph {#{unl $ concatMap graph (unGLM results)}}|]
  where
  unl s = "\n" ++ unlines (map ("\t" ++) s)

help :: IO ()
help = putStrLn "Usage: glm2dot [FILE]*"

go :: [String] -> IO [ParseResult]
go xs@(_:_) = mapM processFile xs
go []       = fmap return $ getContents >>= processContents "<STDIN>"

processFile :: String -> IO ParseResult
processFile f = readFile f >>= processContents f

processContents :: String -> String -> IO ParseResult
processContents f c = return $ P.parse glmParser f c

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
