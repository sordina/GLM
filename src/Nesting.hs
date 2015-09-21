{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Nesting where

import Parser2
import Data.Maybe
import Control.Lens
import Control.Monad.State

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

-- Tests

tests :: Test
tests = $(testGroupGenerator)

-- Properties

prop_flatten :: Bool
prop_flatten = (== 3) $ length $ flatten [Entry ["l1"] [Prop ("name", "n1"), Nested (Entry ["l2"] [])]]

-- Main Nesting Function

flatten :: [Entry] -> [Entry]
flatten entries = evalState (flatPack entries) 0

-- Nesting Helper Functions

catNested :: Entry -> [Entry]
catNested = toListOf (contents . each . _Nested)

addParent :: String -> Entry -> Entry
addParent p = over contents (++ [Prop ("parent",p)])

addName :: String -> Entry -> Entry
addName n = over contents (++ [Prop ("name",n)])

phantomLink :: String -> String -> Entry
phantomLink f t = Entry ["object","link"] [Prop ("name", "nl_"), Prop ("from", f), Prop ("to", t)]

flatPack :: [Entry] -> State Int [Entry]
flatPack es = do
  r <- mapM unNest es
  return $ (map stripNested es) ++ concat r

-- TODO: This could be better...
stripNested :: Entry -> Entry
stripNested e = set contents (map Prop (e ^.. contents . each . _Prop)) e

unNest :: Entry -> State Int [Entry]
unNest e = do
  r <- mapM (fabulate e) (catNested e)
  flatPack $ concat r

fabulate :: Entry -> Entry -> State Int [Entry]
fabulate p c = do
  modify succ
  s <- get
  let cname = getType c ++ show s ++ "_"
  return [c & addName cname & addParent pname, phantomLink pname cname]
  where
  pname = getName p

getName :: Entry -> String
getName = fromMaybe "unnamed" . lookup "name" . toListOf (contents . each . _Prop)

getType :: Entry -> String
getType (Entry (_:t:_) _) = t
getType _                 = "unknown"
