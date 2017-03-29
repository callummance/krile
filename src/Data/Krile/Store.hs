{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- |Module for database access handling
module Data.Krile.Store
  where


import Control.Exception
import Control.Monad
import Control.Monad.IO.Class           (liftIO, MonadIO)
import Control.Monad.Trans.Reader       (ReaderT)
import Control.Monad.Logger             (NoLoggingT)
import Control.Monad.Trans.Resource.Internal
import Control.Monad.Trans.Control
import Data.Text                        (Text, pack)
import Data.Maybe
import Data.Word
import Data.Krile.Sharlayan.User        (Character(..))
import Data.Krile.Sharlayan.FreeCompany (FreeCompany(..))
import Data.Time.Clock
import Data.Configurator
import Data.Configurator.Types
import Database.Persist 
import Database.Persist.Sqlite
import Database.Persist.TH
import Database.Persist.Sql.Types.Internal


-- |Data type storing the successful or unsuccessful result of a character search
data SearchResult = NotFound                    -- ^ No matches were found
                  | SearchError String          -- ^ An error occurred during the search
                  | ManyCharsFound [Character]  -- ^ More than one matches were found for the search
                  | OneCharFound Character      -- ^ Exactly one character matched the search requirements
                  | FcFound FreeCompany         -- ^ FC data was successfully parsed
  deriving (Show)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
FcStore
  Id      String  sql=id
  name    String
  world   String
  profile String
  message String
  members Int
  wRank   Int
  mRank   Int
  updated UTCTime
CharStore 
  Id      Int     sql=id
  name    String
  world   String
  face    String
  profile String
  updated UTCTime
  fc      String Maybe
|]

-- |Creates a new FcStore given an FreeCompany object
fcToStore :: FreeCompany -> FcStore
fcToStore f 
  = FcStore (fcName f) (fcWorld f) (fcURL f) (cBoard f) (members f) (snd $ ranks f) (fst $ ranks f) (tUpdated f)

-- |Creates a new FreeCompany given an FcStore and ID
storeToFc :: FcStore -> Integer -> FreeCompany
storeToFc f id
  = FreeCompany id (fcStoreName f) (fcStoreWorld f) (fcStoreProfile f) (fcStoreUpdated f) (fcStoreMessage f) (fcStoreMembers f) (fcStoreWRank f, fcStoreMRank f) 

-- |Creates a new CharStore given a Character object
charToStore :: Character -> CharStore
charToStore c = CharStore (name c) (world c) (face c) (profile c) (time c) (liftM show (fc c))

-- |Creates a new CharStore given a Character object and ID
storeToChar :: CharStore -> Int -> Character
storeToChar c id
  = Character id (charStoreName c) (charStoreWorld c) (charStoreFace c) (charStoreProfile c) (charStoreUpdated c) (liftM read (charStoreFc c))

-- |Helper function for running database actions
runDb :: (MonadBaseControl IO m, MonadIO m) => Config -> ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
runDb conf action
  = do
    dbLoc <- liftIO $ do
      dbFile :: Maybe String <- Data.Configurator.lookup conf "dbFile"
      case dbFile of
        Nothing     -> error "Config file was missing some required fields"
        Just str    -> return str
    runSqlite (pack dbLoc) $ do 
      runMigration migrateAll 
      res <- action
      return res

-- |Repserts a Character object into the database
insertCharacter :: (MonadBaseControl IO m, MonadIO m) => Character -> Config -> m ()
insertCharacter c conf
  = do
    let charStore = CharStore (name c) (world c) (face c) (profile c) (time c) (liftM show (fc c))
    runDb conf $ do
      id <- repsert (CharStoreKey $ uid c) charStore
      return ()

-- |Repserts a Free Company object into the database
insertFc :: (MonadBaseControl IO m, MonadIO m) => FreeCompany -> Config -> m ()
insertFc f conf
  = runDb conf $ do
    id <- repsert (FcStoreKey . show $ fid f) $ fcToStore f
    return ()


-- |Attempts to retrieve an FC from the database given the ID and config
getFc :: Integer -> Config -> IO SearchResult
getFc f conf
  = do
    fcStore <- try $ runDb conf $ do
      get (FcStoreKey $ show f)
    return $ case fcStore of
      Left (err :: IOException) -> SearchError "Encountered error whilst looking up Free Company in database. Please contact an administrator."
      Right Nothing             -> NotFound
      Right (Just fs)           -> FcFound $ storeToFc fs f

-- |Attempts to retrieve a character from the database given the name, world and a config object
getChar:: String -> String -> Config -> IO SearchResult
getChar name world conf
  = do
    charData <- try $ runDb conf $ do
      selectList [CharStoreName ==. name, CharStoreWorld ==. world] []
    searchResult <- return $ case charData of
      Left (err :: IOException)   -> SearchError "Encountered error whilst looking up Character in database. Please contact an administrator."
      Right []                    -> NotFound
      Right [Entity id char]      -> OneCharFound $ storeToChar char $ unCharStoreKey id
      Right (c:cs)                -> ManyCharsFound $ map (\(Entity id char) -> storeToChar char $ unCharStoreKey id) (c:cs)
    return NotFound
