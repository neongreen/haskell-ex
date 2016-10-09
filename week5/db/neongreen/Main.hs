{-# LANGUAGE NoImplicitPrelude #-}


module Main where


import BasePrelude
-- Monads
import Control.Monad.State
-- Text
import qualified Data.Text as T
import Data.Text (Text)
-- Numbers
import Data.Scientific
-- Containers
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>))
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map)


----------------------------------------------------------------------------
-- DB types
----------------------------------------------------------------------------

-- | Values that can be in table rows.
data Value = Null | Number Scientific | String Text
  deriving (Eq, Ord, Show)

-- | Constraints that can be put on columns.
data Constraint = Unique | NotNull
  deriving (Eq, Ord, Show)

data Column = Column {
  columnName :: Text,
  columnConstraints :: Set Constraint,
  columnData :: Seq Value }
  deriving (Show)

data Table = Table {
  tableColumns :: [Column] }
  deriving (Show)

-- | State of the database.
data Db = Db {
  dbTables :: Map Text Table }
  deriving (Show)

data DbError
  -- | User asked to create a table with duplicate column names
  = DuplicateColumnNames
  -- | Table not found
  | TableNotFound Text
  -- | Inserted row's length doesn't match number of columns (params:
  -- expected number, actual number)
  | RowLengthMismatch Int Int
  -- | A constraint was violated when inserting values (params: column name,
  -- violated constraint, inserted value)
  | ConstraintViolation Text Constraint Value
  deriving (Eq, Show)

----------------------------------------------------------------------------
-- Operations
----------------------------------------------------------------------------

-- | Create a table in the database.
createTable_
  :: Text                     -- ^ Table name
  -> [(Text, [Constraint])]   -- ^ Column names and constraints
  -> Db
  -> Either DbError Db
createTable_ tname cols db = do
  -- If there are any duplicate column names, we error out
  when (length (nub (map fst cols)) /= length (map fst cols)) $
    Left DuplicateColumnNames
  -- Otherwise, let's define 'Column's and a 'Table'
  let columns :: [Column]
      columns = do
        (c_name, c_constraints) <- cols
        return Column {
          columnName        = c_name,
          columnConstraints = S.fromList c_constraints,
          columnData        = mempty }
  let table :: Table
      table = Table {
        tableColumns = columns }
  -- Now that we have a table, we can add it to the database
  return Db {
    dbTables = M.insert tname table (dbTables db) }

-- | Insert a record into the database.
insert_
  :: Text           -- ^ Table name
  -> [Value]        -- ^ Row
  -> Db
  -> Either DbError Db
insert_ tname row db = do
  -- Various checks
  table <- case M.lookup tname (dbTables db) of
    Nothing -> Left (TableNotFound tname)
    Just t  -> return t
  when (length row /= length (tableColumns table)) $
    Left $ RowLengthMismatch (length (tableColumns table)) (length row)
  -- Constraint check
  for_ (zip (tableColumns table) row) $ \(col, v) ->
    for_ (columnConstraints col) $ \constr -> do
      let violated = case constr of
            NotNull -> v == Null
            Unique  -> v /= Null && any (== v) (columnData col)
      when violated $
        Left $ ConstraintViolation tname constr v
  -- Can insert now
  let insertIntoColumn :: Column -> Value -> Column
      insertIntoColumn col v = col {columnData = columnData col |> v}
  let table' = table {
        tableColumns = zipWith insertIntoColumn (tableColumns table) row }
  return Db {
    dbTables = M.insert tname table' (dbTables db) }
