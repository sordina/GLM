{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework

import qualified Parser2   as P
import qualified Tokenizer as T
import qualified Nesting   as N

main :: IO ()
main = defaultMain
  [ testGroup "Parser"    [ P.tests ]
  , testGroup "Tokenizer" [ T.tests ]
  , testGroup "Nesting"   [ N.tests ]
  ]

