{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- |Processing of data from the lodestone and database
module Data.Krile.FFData
  where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Configurator.Types
import Data.Krile.Sharlayan.User
import Data.Krile.Sharlayan.News
import Data.Krile.Sharlayan.FreeCompany
import Data.Krile.Store

-- |Fetches a character from the lodestone and writes it to the data store given the name, world and config
fetchCharData :: String -> String -> Config -> IO SearchResult
fetchCharData name world conf
  = do
    res <- liftIO $ do
      charsFound <- try $ findChar name world
      searchResult <- case charsFound of
        Left (err :: IOException) -> return $ SearchError "Encountered error whilst looking up character. Please try again."
        Right []                  -> return $ NotFound
        Right [c]                 -> return $ OneCharFound c
        Right (c:cs)              -> return $ ManyCharsFound (c:cs)
      case searchResult of
        OneCharFound c            -> liftM OneCharFound $ expandCharacter c
        other                     -> return other
    case res of
      OneCharFound c    -> insertCharacter c conf >> case (fc c) of {Just fid -> fetchFcData fid conf; Nothing -> return NotFound} >> return res
      other             -> return other

-- |Fetches FC data from the lodestone and writes it to the data store given the ID and a config
fetchFcData :: Integer -> Config -> IO SearchResult
fetchFcData id conf
  = do
    res <- liftIO $ do
      found <- try $ fetchFC id
      case found of 
        Left (err :: IOException) -> return $ SearchError "Encountered error whilst looking up Free Company. Try again in a bit."
        Right fc                  -> return $ FcFound fc
    case res of
      FcFound fc                  -> insertFc fc conf >> return res
      other                       -> return other
