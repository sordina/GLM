{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework

import qualified GLM.Parser    as P
import qualified GLM.Tokenizer as T
import qualified GLM.Nesting   as N

main :: IO ()
main = defaultMain
  [ testGroup "Parser"    [ P.tests ]
  , testGroup "Tokenizer" [ T.tests ]
  , testGroup "Nesting"   [ N.tests ]
  ]

