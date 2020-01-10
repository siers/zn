{-# LANGUAGE GADTs, TypeFamilies, TemplateHaskell, QuasiQuotes, FlexibleInstances, StandaloneDeriving, FlexibleContexts #-}

module Zn.Persist where

import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Database.Groundhog as G
import Database.Groundhog.Core
import Database.Groundhog.Sqlite as GS
import Database.Groundhog.TH
import Data.Text (Text)
import Zn.Types

data Fact = Fact
    { factName :: Text
    , factValue :: Text
    , factSecret :: Maybe Text
    } deriving (Eq, Show)

data TgTarget = TgTarget
    { tgTargetKey :: Int
    , tgTargetChannel :: Text
    } deriving (Eq, Show)

mkPersist defaultCodegenConfig [groundhog|
- entity: Fact
  constructors:
    - name: Fact
      uniques:
        - name: NameConstraint
          fields: [factName]
- entity: TgTarget
  keys:
    - name: tg_target_key_column
  constructors:
    - name: TgTarget
      uniques:
        - name: tg_target_key_column
          fields: [tgTargetKey]
|]

sql :: (MonadBaseControl IO m, MonadIO m) => Action Sqlite a -> m a
sql = withSqliteConn dbString . runDbConn

runZnMigrations :: String -> IO ()
runZnMigrations dbString = withSqliteConn dbString . runDbConn $ do
    runMigration $ do
        migrate (undefined :: Fact)
        migrate (undefined :: TgTarget)

-- dbtest :: PersistBackend m => m ()
-- dbtest = GS.insert (Fact "fact:ping" "pong") >> return ()
