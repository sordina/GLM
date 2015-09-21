{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Nesting where

import Parser2
import Data.Maybe
import Control.Lens
import Control.Monad.State

prop_flatten = print $ flatten [Entry ["l1"] [Prop ("name", "n1"), Nested (Entry ["l2"] [])]]

main = prop_flatten

catNested :: Entry -> [Entry]
catNested = toListOf (contents . each . _Nested)

addParent :: String -> Entry -> Entry
addParent p = over contents (++ [Prop ("parent",p)])

addName :: String -> Entry -> Entry
addName n = over contents (++ [Prop ("name",n)])

phantomLink :: String -> String -> Entry
phantomLink f t = Entry ["object","link"] [Prop ("name", "nl_"), Prop ("from", f), Prop ("to", t)]

flatten :: [Entry] -> [Entry]
flatten entries = evalState (flatPack entries) 0

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
  let cname = "unnested_" ++ show s
  return [c & addName cname & addParent pname, phantomLink pname cname]
  where
  pname = getName p

getName :: Entry -> String
getName = fromMaybe "unnamed" . lookup "name" . toListOf (contents . each . _Prop)
