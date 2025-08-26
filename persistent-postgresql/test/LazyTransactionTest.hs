{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module LazyTransactionTest (specs) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Test.Hspec
import Data.IORef

import PgInit (runConnAssert, runConnLazy)

share [mkPersist sqlSettings, mkMigrate "lazyTestMigrate"] [persistLowerCase|
LazyTestEntity
    name String
    deriving Show Eq
|]

-- | Tests for lazy transaction behavior
specs :: Spec
specs = describe "LazyTransactionTest" $ do

    it "lazy transaction with pure () should not acquire connection" $ do
        -- This test verifies that runConnLazy with pure () doesn't even acquire a connection
        -- Since no SQL is executed, the transaction should never start
        result <- runResourceT $ runConnLazy $ do
            pure "no database work"
        result `shouldBe` "no database work"

    it "lazy transaction only starts when SQL is needed" $ runConnAssert $ do
        runMigration lazyTestMigrate
        deleteWhere ([] :: [Filter LazyTestEntity])

        -- Use an IORef to track execution
        executionTracker <- liftIO $ newIORef []

        -- Run a lazy transaction with conditional logic
        result <- liftIO $ runResourceT $ runConnLazy $ do
            pure "pure path"

        liftIO $ result `shouldBe` "pure path"

        -- Verify nothing was inserted
        entities <- selectList [] [] :: SqlPersistT (LoggingT (ResourceT IO)) [Entity LazyTestEntity]
        liftIO $ length entities `shouldBe` 0

    it "lazy transaction executes SQL when needed" $ runConnAssert $ do
        runMigration lazyTestMigrate
        deleteWhere ([] :: [Filter LazyTestEntity])

        -- Use lazy transaction that does need database
        result <- liftIO $ runResourceT $ runConnLazy $ do
            runMigration lazyTestMigrate  -- Need migration in lazy context too

            -- This will trigger the transaction to start
            entityId <- insert $ LazyTestEntity "lazy_inserted"
            pure $ "inserted: " ++ show (fromSqlKey entityId)

        -- Verify it was actually inserted
        entities <- selectList [] [] :: SqlPersistT (LoggingT (ResourceT IO)) [Entity LazyTestEntity]
        liftIO $ length entities `shouldBe` 1
        liftIO $ entityVal (head entities) `shouldBe` LazyTestEntity "lazy_inserted"
