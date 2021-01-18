{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestImport
  ( module TestImport,
    module X,
  )
where

import Application (handler, makeFoundation, makeLogWare)
import ClassyPrelude as X hiding (Handler, delete, deleteBy)
import Control.Monad.Logger (runLoggingT)
import Database.Persist as X hiding (get)
import Database.Persist.Sql (SqlPersistM, connEscapeName, rawExecute, rawSql, runSqlPersistMPool, unSingle)
import Database.Persist.Sqlite (createSqlitePoolFromInfo, fkEnabled, mkSqliteConnectionInfo, sqlDatabase)
import Foundation as X
import Graphql.API (getApi)
import Lens.Micro (set)
import Model as X
import Settings (appDatabaseConf)
import Test.Hspec as X
import Yesod.Core (messageLoggerSource)
import Yesod.Core.Unsafe (fakeHandlerGetLogger)
import Yesod.Default.Config2 (loadYamlSettings, useEnv)
import Yesod.Test as X

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
  pool <- fmap appConnPool getTestYesod
  liftIO $ runSqlPersistMPool query pool

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
  app <- getTestYesod
  fakeHandlerGetLogger appLogger app handler

withApp :: SpecWith (TestApp App) -> Spec
withApp = before $ do
  settings <-
    loadYamlSettings
      ["config/test-settings.yml", "config/settings.yml"]
      []
      useEnv
  morpheus <- handler getApi
  foundation <- makeFoundation settings morpheus
  wipeDB foundation
  logWare <- liftIO $ makeLogWare foundation
  return (foundation, logWare)

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = do
  -- In order to wipe the database, we need to use a connection which has
  -- foreign key checks disabled.  Foreign key checks are enabled or disabled
  -- per connection, so this won't effect queries outside this function.
  --
  -- Aside: foreign key checks are enabled by persistent-sqlite, as of
  -- version 2.6.2, unless they are explicitly disabled in the
  -- SqliteConnectionInfo.

  let logFunc = messageLoggerSource app (appLogger app)

  let dbName = sqlDatabase $ appDatabaseConf $ appSettings app
      connInfo = set fkEnabled False $ mkSqliteConnectionInfo dbName

  pool <- runLoggingT (createSqlitePoolFromInfo connInfo 1) logFunc

  flip runSqlPersistMPool pool $ do
    tables <- getTables
    sqlBackend <- ask
    let queries = map (\t -> "DELETE FROM " ++ (connEscapeName sqlBackend $ DBName t)) tables
    forM_ queries (\q -> rawExecute q [])

getTables :: DB [Text]
getTables = do
  tables <- rawSql "SELECT name FROM sqlite_master WHERE type = 'table';" []
  return (fmap unSingle tables)
