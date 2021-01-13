{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Migrate
import Database.Beam.Migrate.SQL.BeamExtensions
import Database.Beam.Migrate.Simple
import Database.Beam.Sqlite
import Database.Beam.Sqlite.Migrate
import Database.SQLite.Simple (open)
import GHC.Int

data ReproT f = Repro {
  _id :: Columnar f (SqlSerial Int64)
  , _refCount :: Columnar f Int32
  } deriving (Generic, Beamable)

instance Table ReproT where
  data PrimaryKey ReproT f = ReproId (Columnar f (SqlSerial Int64)) deriving (Generic, Beamable)
  primaryKey = ReproId . _id

reproMigration :: Migration Sqlite (CheckedDatabaseEntity Sqlite db (TableEntity ReproT))
reproMigration = createTable "repro" $ Repro {
  _id = genericSerial "id"
  , _refCount = field "ref_count" int notNull (defaultTo_ (val_ 0))
  }

data Db f = Db {
  _repro :: f (TableEntity ReproT)
} deriving Generic

deriving instance (Database Sqlite) Db

initialSetup :: Migration Sqlite (CheckedDatabaseSettings Sqlite Db)
initialSetup = Db <$> reproMigration

initialSetupStep :: MigrationSteps Sqlite () (CheckedDatabaseSettings Sqlite Db)
initialSetupStep = migrationStep "initial_setup" (const initialSetup)

allowDestructive :: (Monad m, MonadFail m) => BringUpToDateHooks m
allowDestructive = defaultUpToDateHooks { runIrreversibleHook = pure True }

main :: IO ()
main = do
  conn <- open "test.db"
  result <- runBeamSqliteDebug putStrLn conn $
    bringUpToDateWithHooks allowDestructive migrationBackend initialSetupStep >>= \case
      Nothing -> return $ Left "Sqlite migration failed"
      Just (_ :: (CheckedDatabaseSettings Sqlite Db)) -> return $ Right ()
  putStrLn ("Migrate result: " <> show result)
