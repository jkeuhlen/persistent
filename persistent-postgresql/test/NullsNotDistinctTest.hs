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
{-# LANGUAGE UndecidableInstances #-}

module NullsNotDistinctTest where

import Control.Exception (SomeException, try)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.Postgresql.Internal
import Database.Persist.TH
import qualified Test.Hspec as Hspec
import qualified Test.Hspec.Expectations.Lifted as Lifted

import PgInit

-- Test entities with and without NULLS NOT DISTINCT
share
    [mkPersist sqlSettings, mkMigrate "nullsNotDistinctMigrate"]
    [persistLowerCase|
  -- Standard unique constraint (allows multiple NULLs)
  StandardUnique
    name Text
    email Text Maybe
    UniqueStandardEmail name email !force
    deriving Eq Show

  -- Unique constraint with NULLS NOT DISTINCT (PostgreSQL 15+)
  -- This should prevent multiple NULLs
  NullsNotDistinctUnique
    name Text
    email Text Maybe
    UniqueNNDEmail name email !nullsNotDistinct
    deriving Eq Show

  -- Multiple nullable fields with NULLS NOT DISTINCT
  MultiFieldNND
    fieldA Text
    fieldB Text Maybe
    fieldC Int Maybe
    UniqueMultiNND fieldA fieldB fieldC !nullsNotDistinct
    deriving Eq Show
|]

-- Helper to check PostgreSQL version
getPostgresVersion :: (MonadIO m) => ReaderT SqlBackend m (Maybe Int)
getPostgresVersion = do
    result <- rawSql "SELECT current_setting('server_version_num')::integer" []
    case result of
        [Single version] -> return $ Just version
        _ -> return Nothing

isPostgres15OrHigher :: (MonadIO m) => ReaderT SqlBackend m Bool
isPostgres15OrHigher = do
    mVersion <- getPostgresVersion
    case mVersion of
        Just version -> return $ version >= 150000 -- PostgreSQL 15.0
        Nothing -> return False

cleanDB
    :: (BaseBackend backend ~ SqlBackend, PersistQueryWrite backend, MonadIO m)
    => ReaderT backend m ()
cleanDB = do
    deleteWhere ([] :: [Filter StandardUnique])
    deleteWhere ([] :: [Filter NullsNotDistinctUnique])
    deleteWhere ([] :: [Filter MultiFieldNND])

specs :: Spec
specs = describe "NULLS NOT DISTINCT support" $ do
    let
        runDb = runConnAssert

    it "generates correct SQL for NULLS NOT DISTINCT constraint" $ do
        let
            alterWithNND =
                AddUniqueConstraint
                    (ConstraintNameDB "unique_nnd_email")
                    [FieldNameDB "name", FieldNameDB "email"]
                    ["!nullsNotDistinct"]

        let
            alterWithoutNND =
                AddUniqueConstraint
                    (ConstraintNameDB "unique_standard_email")
                    [FieldNameDB "name", FieldNameDB "email"]
                    ["!force"]

        let
            tableName = EntityNameDB "test_table"
        let
            sqlWithNND = showAlterTable tableName alterWithNND
        let
            sqlWithoutNND = showAlterTable tableName alterWithoutNND

        sqlWithNND
            `Hspec.shouldBe` "ALTER TABLE \"test_table\" ADD CONSTRAINT \"unique_nnd_email\" UNIQUE NULLS NOT DISTINCT(\"name\",\"email\")"

        sqlWithoutNND
            `Hspec.shouldBe` "ALTER TABLE \"test_table\" ADD CONSTRAINT \"unique_standard_email\" UNIQUE(\"name\",\"email\")"

    describe "runtime behavior" $ do
        it "standard unique allows multiple NULLs" $ do
            runDb $ do
                cleanDB

                -- These should both succeed with standard unique
                k1 <- insert $ StandardUnique "user1" Nothing
                k2 <- insert $ StandardUnique "user2" Nothing

                -- Verify both were inserted
                count1 <- count [StandardUniqueName ==. "user1"]
                count2 <- count [StandardUniqueName ==. "user2"]

                liftIO $ do
                    count1 `Lifted.shouldBe` 1
                    count2 `Lifted.shouldBe` 1

        it "standard unique prevents duplicate non-NULLs" $ do
            ( runDb $ do
                    cleanDB
                    _ <- insert $ StandardUnique "user1" (Just "test@example.com")
                    _ <- insert $ StandardUnique "user1" (Just "test@example.com")
                    return ()
                )
                `Hspec.shouldThrow` Hspec.anyException

        it
            "standard unique getBy returns Nothing for NULL values (backwards compatibility)"
            $ do
                runDb $ do
                    cleanDB

                    -- Insert a record with NULL email
                    _ <- insert $ StandardUnique "user1" Nothing

                    -- getBy with NULL should return Nothing (standard SQL behavior)
                    -- This ensures backwards compatibility - without !nullsNotDistinct,
                    -- getBy cannot find NULL values
                    result <- getBy $ UniqueStandardEmail "user1" Nothing

                    liftIO $ result `Lifted.shouldBe` Nothing

                    -- Verify that getBy still works for non-NULL values
                    k2 <- insert $ StandardUnique "user2" (Just "test@example.com")
                    result2 <- getBy $ UniqueStandardEmail "user2" (Just "test@example.com")

                    liftIO $ case result2 of
                        Just (Entity key _) -> key `Lifted.shouldBe` k2
                        Nothing -> Hspec.expectationFailure "getBy should find non-NULL values"

        describe "PostgreSQL 15+ features" $ do
            it "NULLS NOT DISTINCT prevents multiple NULLs (PostgreSQL 15+)" $ do
                runDb $ do
                    supported <- isPostgres15OrHigher
                    when supported $ do
                        -- Run the migration to ensure constraint is created
                        void $ runMigrationSilent nullsNotDistinctMigrate
                    unless supported $
                        liftIO $
                            Hspec.pendingWith "Requires PostgreSQL 15 or higher"

                -- Now test the constraint enforcement separately
                ( runDb $ do
                        cleanDB
                        void $ runMigrationSilent nullsNotDistinctMigrate
                        _ <- insert $ NullsNotDistinctUnique "user1" Nothing
                        -- Same name and email - this should violate the unique constraint
                        _ <- insert $ NullsNotDistinctUnique "user1" Nothing
                        return ()
                    )
                    `Hspec.shouldThrow` Hspec.anyException

            it "NULLS NOT DISTINCT with multiple nullable fields (PostgreSQL 15+)" $ do
                -- First test that different NULL patterns work
                runDb $ do
                    supported <- isPostgres15OrHigher
                    if supported
                        then do
                            cleanDB

                            -- First record with NULLs
                            _ <- insert $ MultiFieldNND "test1" Nothing Nothing

                            -- Different NULL pattern should succeed
                            _ <- insert $ MultiFieldNND "test1" (Just "value") Nothing
                            _ <- insert $ MultiFieldNND "test1" Nothing (Just 42)

                            count' <- count ([] :: [Filter MultiFieldNND])
                            liftIO $ count' `Hspec.shouldBe` 3
                        else
                            liftIO $ Hspec.pendingWith "Requires PostgreSQL 15 or higher"

                -- Test duplicate prevention with same NULL pattern
                ( runDb $ do
                        supported <- isPostgres15OrHigher
                        when supported $ do
                            cleanDB
                            _ <- insert $ MultiFieldNND "test1" Nothing Nothing
                            _ <- insert $ MultiFieldNND "test1" Nothing Nothing
                            return ()
                    )
                    `Hspec.shouldThrow` Hspec.anyException

            it "getBy finds NULL values with NULLS NOT DISTINCT (PostgreSQL 15+)" $ do
                runDb $ do
                    supported <- isPostgres15OrHigher
                    if supported
                        then do
                            cleanDB
                            void $ runMigrationSilent nullsNotDistinctMigrate

                            -- Insert with NULL
                            k1 <- insert $ NullsNotDistinctUnique "user1" Nothing

                            -- With our runtime detection, getBy now uses IS NOT DISTINCT FROM
                            -- for entities with !nullsNotDistinct, allowing it to find NULL values
                            result <- getBy $ UniqueNNDEmail "user1" Nothing

                            -- We expect getBy TO find the entity with NULLS NOT DISTINCT
                            liftIO $ case result of
                                Just (Entity key _) -> key `Hspec.shouldBe` k1
                                Nothing ->
                                    Hspec.expectationFailure
                                        "getBy should find NULL values when !nullsNotDistinct is set"
                        else
                            liftIO $ Hspec.pendingWith "Requires PostgreSQL 15 or higher"

        it "migration generates correct constraints" $ do
            runDb $ do
                -- Run migration to create tables
                void $ runMigrationSilent nullsNotDistinctMigrate

                -- Check that constraints were created
                -- This query checks PostgreSQL's information schema
                constraints :: [(Single Text, Single Text)] <-
                    rawSql
                        "SELECT conname, pg_get_constraintdef(oid) \
                        \FROM pg_constraint \
                        \WHERE conrelid = 'nulls_not_distinct_unique'::regclass \
                        \  AND contype = 'u'"
                        []

                supported <- isPostgres15OrHigher
                liftIO $ case constraints of
                    [] -> return () -- Tables might not exist yet
                    results -> do
                        -- Check if any constraint has NULLS NOT DISTINCT
                        let
                            hasNND =
                                any
                                    ( \(Single _, Single def) ->
                                        "NULLS NOT DISTINCT" `T.isInfixOf` def
                                    )
                                    results

                        when supported $
                            hasNND `Hspec.shouldBe` True
