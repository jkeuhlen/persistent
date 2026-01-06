{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module LazyTransactionSpec where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import System.Environment (getEnvironment)
import Test.Hspec
import UnliftIO (MonadIO, liftIO)

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.Sql
import Database.Persist.TH

import Init (isTravis)

-- | Check for docker postgres environment variable
dockerPg :: IO (Maybe BS.ByteString)
dockerPg = do
    env <- getEnvironment
    return $ case lookup "POSTGRES_NAME" env of
        Just _name -> Just "postgres"
        _ -> Nothing

share
    [mkPersist sqlSettings{mpsGeneric = False}]
    [persistLowerCase|
LazyTxTestEntity sql=lazy_tx_test_entity
    name Text
    value Int
    deriving Show Eq
|]

lazyTxTestMigrate :: Migration
lazyTxTestMigrate = migrateModels [entityDef (Proxy :: Proxy LazyTxTestEntity)]

-- | Get connection string based on environment
getConnString :: (MonadIO m) => m BS.ByteString
getConnString = do
    travis <- liftIO isTravis
    if travis
        then pure "host=localhost port=5432 user=perstest password=perstest dbname=persistent"
        else do
            host <- fromMaybe "localhost" <$> liftIO dockerPg
            pure ("host=" <> host <> " port=5432 user=postgres dbname=test")

-- | Run with lazy transactions enabled
runConnLazyTx :: SqlPersistT (LoggingT (ResourceT IO)) a -> IO a
runConnLazyTx f = do
    connString <- getConnString
    runResourceT $ flip runLoggingT (\_ _ _ _ -> pure ()) $ do
        let
            conf =
                PostgresConf
                    { pgConnStr = connString
                    , pgPoolStripes = 1
                    , pgPoolIdleTimeout = 60
                    , pgPoolSize = 1
                    }
            hooks = defaultPostgresConfHooks
                { pgConfHooksUseLazyTransactions = True
                }
        withPostgresqlPoolWithConf conf hooks $ \pool -> do
            runSqlPool f pool

-- | Run with lazy transactions disabled (default behavior)
runConnNoLazyTx :: SqlPersistT (LoggingT (ResourceT IO)) a -> IO a
runConnNoLazyTx f = do
    connString <- getConnString
    runResourceT $ flip runLoggingT (\_ _ _ _ -> pure ()) $ do
        let
            conf =
                PostgresConf
                    { pgConnStr = connString
                    , pgPoolStripes = 1
                    , pgPoolIdleTimeout = 60
                    , pgPoolSize = 1
                    }
            hooks = defaultPostgresConfHooks
                { pgConfHooksUseLazyTransactions = False
                }
        withPostgresqlPoolWithConf conf hooks $ \pool -> do
            runSqlPool f pool

cleanDB :: (MonadIO m) => SqlPersistT m ()
cleanDB = rawExecute "DROP TABLE IF EXISTS lazy_tx_test_entity" []

setupDB :: (MonadUnliftIO m) => SqlPersistT m ()
setupDB = do
    cleanDB
    _ <- runMigrationSilent lazyTxTestMigrate
    pure ()

spec :: Spec
spec = describe "LazyTransactionSpec" $ do
    describe "with lazy transactions enabled" $ do
        it "handles single statement operations correctly" $ do
            runConnLazyTx $ do
                setupDB
                -- Single insert should work via autocommit
                _ <- insert $ LazyTxTestEntity "test1" 1
                -- Verify it was persisted
                entities <- selectList @LazyTxTestEntity [] []
                liftIO $ length entities `shouldBe` 1
                cleanDB

        it "handles multiple statement operations correctly" $ do
            runConnLazyTx $ do
                setupDB
                -- Multiple inserts should trigger BEGIN before second
                _ <- insert $ LazyTxTestEntity "test1" 1
                _ <- insert $ LazyTxTestEntity "test2" 2
                _ <- insert $ LazyTxTestEntity "test3" 3
                -- Verify all were persisted
                entities <- selectList @LazyTxTestEntity [] []
                liftIO $ length entities `shouldBe` 3
                cleanDB

        it "maintains data consistency with updates" $ do
            runConnLazyTx $ do
                setupDB
                key <- insert $ LazyTxTestEntity "original" 100
                update key [LazyTxTestEntityName =. "updated"]
                mEntity <- get key
                liftIO $ case mEntity of
                    Just e -> lazyTxTestEntityName e `shouldBe` "updated"
                    Nothing -> expectationFailure "Entity not found"
                cleanDB

        it "handles delete operations" $ do
            runConnLazyTx $ do
                setupDB
                key <- insert $ LazyTxTestEntity "to-delete" 999
                delete key
                mEntity <- get key
                liftIO $ mEntity `shouldBe` Nothing
                cleanDB

    describe "with lazy transactions disabled (default)" $ do
        it "handles single statement operations correctly" $ do
            runConnNoLazyTx $ do
                setupDB
                _ <- insert $ LazyTxTestEntity "test1" 1
                entities <- selectList @LazyTxTestEntity [] []
                liftIO $ length entities `shouldBe` 1
                cleanDB

        it "handles multiple statement operations correctly" $ do
            runConnNoLazyTx $ do
                setupDB
                _ <- insert $ LazyTxTestEntity "test1" 1
                _ <- insert $ LazyTxTestEntity "test2" 2
                entities <- selectList @LazyTxTestEntity [] []
                liftIO $ length entities `shouldBe` 2
                cleanDB

    describe "comparing lazy vs non-lazy behavior" $ do
        it "both modes produce same results for multi-statement transactions" $ do
            -- Test with lazy tx
            lazyResult <- runConnLazyTx $ do
                setupDB
                _ <- insert $ LazyTxTestEntity "a" 1
                _ <- insert $ LazyTxTestEntity "b" 2
                _ <- insert $ LazyTxTestEntity "c" 3
                entities <- selectList @LazyTxTestEntity [] [Asc LazyTxTestEntityName]
                cleanDB
                pure $ map (lazyTxTestEntityName . entityVal) entities

            -- Test with non-lazy tx
            nonLazyResult <- runConnNoLazyTx $ do
                setupDB
                _ <- insert $ LazyTxTestEntity "a" 1
                _ <- insert $ LazyTxTestEntity "b" 2
                _ <- insert $ LazyTxTestEntity "c" 3
                entities <- selectList @LazyTxTestEntity [] [Asc LazyTxTestEntityName]
                cleanDB
                pure $ map (lazyTxTestEntityName . entityVal) entities

            lazyResult `shouldBe` nonLazyResult
            lazyResult `shouldBe` ["a", "b", "c"]
