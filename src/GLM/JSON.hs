
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GLM.JSON where

import Data.Aeson
import Data.String
import Control.Lens
import Control.Arrow
import GLM.Parser
import System.Environment
import System.Exit
import System.IO
import qualified Data.ByteString.Lazy.Char8 as B

instance ToJSON Entry where
  toJSON (Entry a b) = object [ ("path"      , fromString (unwords a))
                              , ("selector"  , toJSON a    )
                              , ("nested"    , toJSON nests)
                              , ("properties", object $ over each (fromString *** fromString) props)]
    where
    props :: [(String, String)]
    props = b ^.. each . _Prop
    nests = b ^.. each . _Nested

main :: IO ()
main = do
  a <- getArgs
  case a of [] -> getContents >>= go "STDIN"
            xs -> mapM_ doF xs

doF :: FilePath -> IO ()
doF f = readFile f >>= go f

go :: String -> String -> IO ()
go f c =
  case glmParser f c of
         Left  err -> oops err
         Right res -> B.putStrLn $ encode $ res

oops :: Show a => a -> IO b
oops err = hPutStrLn stderr (show err) >> exitFailure
