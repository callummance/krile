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
  = do
    let fcStore = FcStore (fcName f) (fcWorld f) (fcURL f) (cBoard f) (members f) (snd $ ranks f) (fst $ ranks f) (tUpdated f)
    runDb conf $ do
      id <- repsert (FcStoreKey . show $ fid f) fcStore
      return ()
