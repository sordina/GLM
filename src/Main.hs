module Main where

import System.Environment
import Parser2

main :: IO ()
main = getArgs >>= start

start :: [String] -> IO ()
start ["-h"]     = help
start ["--help"] = help
start args       = go args >>= mapM_ outputResult

outputResult :: ParseResult -> IO ()
outputResult (Left  issue)   = print "Got an error:" >> print issue
outputResult (Right results) = mapM_ print results

help :: IO ()
help = putStrLn "Usage: glm [FILE]*"

go :: [String] -> IO [ParseResult]
go xs@(_:_) = mapM processFile xs
go []       = fmap return $ getContents >>= processContents "<STDIN>"

processFile :: String -> IO ParseResult
processFile f = readFile f >>= processContents f

processContents :: String -> String -> IO ParseResult
processContents f c = return $ glmParser f c
