module Main where

import Text.Parsec as P
import System.Environment
import Parser

main :: IO ()
main = getArgs >>= start

start :: [String] -> IO ()
start ["-h"]     = help
start ["--help"] = help
start args       = go args >>= mapM_ outputResult

outputResult :: Show a => Either a GLMFile -> IO ()
outputResult (Left  issue)   = print "Got an error:" >> print issue
outputResult (Right results) = mapM_ print $ unGLM results

help :: IO ()
help = putStrLn "Usage: glm [FILE]*"

go :: [String] -> IO [ParseResult]
go xs@(_:_) = mapM processFile xs
go []       = fmap return $ getContents >>= processContents "<STDIN>"

processFile :: String -> IO ParseResult
processFile f = readFile f >>= processContents f

processContents :: String -> String -> IO ParseResult
processContents f c = return $ P.parse glmParser f c
