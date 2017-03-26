{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |Module containing the entry point for the program
module Data.Krile (krile, getConfig)
  where

import Control.Exception
import Data.Krile.FFData
import Data.Configurator
import Data.Configurator.Types
import System.Environment
import System.Exit

-- |A sensible default config file location
defaultConfLoc :: String
defaultConfLoc = "./krile.cfg"

-- |The entry point for the program
krile :: IO ()
krile 
  = do
    args <- getArgs
    conf <- getConfig $ if (length args) > 0 then head args else defaultConfLoc
    exitSuccess

-- |Loads a config file given a location
getConfig :: String -> IO Config
getConfig loc
  = do
    r :: Either IOException Config <- try $ load [ Required loc ]
    conf <- case r of
      Left e      -> error "Encountered error whilst loading config file. Now aborting..."
      Right c     -> return c
    configured :: Maybe Bool <- Data.Configurator.lookup conf "configured"
    case configured of
      Nothing     -> error "The configured flag was not set; please check your config file."
      Just False  -> error "The configured flag was not set; please check your config file."
      _           -> return conf
