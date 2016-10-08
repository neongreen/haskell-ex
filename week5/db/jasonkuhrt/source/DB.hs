{-# LANGUAGE OverloadedStrings #-}

{- README

Create an in-memory database.

-- Level 1

  * commands: create table / insert row / delete row / select row

-}

module DB where

import Data.Text
import Data.Map
import Data.Scientific



type Row = Map Text Value

data ColumnSpec = ColumnSpec {
    name :: Text,
    constraints :: [Constraint]
  }

type Table = Map ColumnSpec [Value]

data Value =
    String Text
  | Number Scientific
  deriving (Show, Eq)

data Constraint =
    NotNull
  | Unique
  deriving (Show, Eq)

type DB = Map Text Table

type DBError = Text



insert :: Text -> Row -> DB -> Either DBError DB
insert tableName row db = undefined

delete :: Text -> (Value -> Bool) -> DB -> Either DBError DB
delete tableName selector db = undefined

select :: Text -> (Value -> Bool) -> DB -> Either DBError DB
select tableName selector db = undefined

createTable :: Text -> [ColumnSpec] -> DB -> DB
createTable tableName spec db = undefined
