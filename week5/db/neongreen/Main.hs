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

type Row = [Value]

-- | Constraints that can be put on columns.
data Constraint = Unique | NotNull
  deriving (Eq, Ord, Show)

data Column = Column {
  columnName :: Text,
  columnConstraints :: Set Constraint,
  columnData :: Seq Value }
  deriving (Show)

data Table = Table {
  tableColumns :: [Column],
  tableRowCount :: Int }
  deriving (Show)

-- | State of the database.
data Db = Db {
  dbTables :: Map Text Table }
  deriving (Show)

data DbError
  -- | User asked to create a table with duplicate column names
  = DuplicateColumnNames
  -- | User asked to return data from columns that aren't present
  | ColumnsNotFound Text [Text]
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
-- Helpers
----------------------------------------------------------------------------

getTable :: Text -> Db -> Either DbError Table
getTable tname db =
  case M.lookup tname (dbTables db) of
    Nothing -> Left (TableNotFound tname)
    Just t  -> return t

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
  -- Check: are there any duplicate column names?
  when (length (nub (map fst cols)) /= length (map fst cols)) $
    Left DuplicateColumnNames
  -- Checks passed; let's construct the table
  let columns :: [Column]
      columns = do
        (c_name, c_constraints) <- cols
        return Column {
          columnName        = c_name,
          columnConstraints = S.fromList c_constraints,
          columnData        = mempty }
  let table :: Table
      table = Table {
        tableColumns  = columns,
        tableRowCount = 0 }
  -- Now that we have a table, we can add it to the database
  return Db {
    dbTables = M.insert tname table (dbTables db) }

-- | Insert a record into the database.
insert_
  :: Text           -- ^ Table name
  -> Row            -- ^ Row
  -> Db
  -> Either DbError Db
insert_ tname row db = do
  table <- getTable tname db
  -- Check: is the length of the row we're inserting the same as the length
  -- of row in the table?
  when (length row /= length (tableColumns table)) $
    Left $ RowLengthMismatch (length (tableColumns table)) (length row)
  -- Check: do inserted values satisfy table constraints?
  for_ (zip (tableColumns table) row) $ \(col, v) ->
    for_ (columnConstraints col) $ \constr -> do
      let violated = case constr of
            NotNull -> v == Null
            Unique  -> v /= Null && any (== v) (columnData col)
      when violated $
        Left $ ConstraintViolation tname constr v
  -- Checks passed, can insert now
  let insertIntoColumn :: Column -> Value -> Column
      insertIntoColumn col v = col {columnData = columnData col |> v}
  let table' = table {
        tableColumns  = zipWith insertIntoColumn (tableColumns table) row,
        tableRowCount = tableRowCount table + 1 }
  return Db {
    dbTables = M.insert tname table' (dbTables db) }

select_
  :: Text            -- ^ Table name
  -> Maybe [Text]    -- ^ Column names, or 'Nothing' to return all columns
  -> Db
  -> Either DbError [Row]
select_ tname mbCols db = do
  table <- getTable tname db
  let columns = tableColumns table
  let tableColNames = map columnName columns
  -- Check: from the column names that the user asked to return, are all
  -- actually present in the table?
  let columnsNotPresent = fromMaybe [] mbCols \\ tableColNames
  unless (null columnsNotPresent) $
    Left $ ColumnsNotFound tname columnsNotPresent
  -- Checks passed, can return the rows
  let colIndices :: [Int]
      colIndices = case mbCols of
        Nothing   -> [0 .. length columns - 1]
        Just cols -> mapMaybe (`elemIndex` tableColNames) cols
  let getRow :: Int -> Row
      getRow i = map (\c -> (columnData (columns !! c)) `Seq.index` i)
                     colIndices
  return (map getRow [0 .. tableRowCount table - 1])
