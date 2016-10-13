module Main where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (nub)
import Data.Text (Text)
import Data.Scientific (Scientific)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Control.Monad.State

data DB = DB {
  dbTables :: Map Text Table
} deriving Show

data Table = Table {
  tableColumns :: [Column]
} deriving Show

data Column = Column {
  colName :: Text,
  constraints :: Set Constraint,
  values :: Seq Value
} deriving Show

data Value = Null | Number Scientific | String Text deriving (Ord, Show, Eq)

data Constraint = NotNull | Unique deriving (Show, Eq, Ord)

data DBError = ConstraintViolated Constraint Text
              | DuplicateColumnNames
              | TableNotFound Text      -- |Table name
              | RowSizeMismatch Int Int -- |Expected size and row size
                deriving (Show, Eq)
                
type Predicate = Value -> Bool 

createTable :: Text -- | Table name
            -> [(Text, [Constraint])]
            -> DB
            -> Either DBError DB
createTable tableName cols db = do
  -- | Remove duplicate column names
  when ((length.nub.map fst $ cols) /= (length.map fst $ cols)) $ 
    Left DuplicateColumnNames
  let columns_ :: [Column]
      columns_ = do
        (colname, cons) <- cols
        return Column {
          colName = colname,
          constraints = S.fromList cons,
          values = mempty}
      table :: Table
      table = Table {
        tableColumns = columns_}
  return DB {dbTables = M.insert tableName table (dbTables db)}

insertIntoCol :: Column -> Value -> Column
insertIntoCol col val = col {
  values = values col |> val}

insert_ :: Text    -- ^Table name
        -> [Value] -- ^Row
        -> DB
        -> Either DBError DB
insert_ tablename vals db = do
  table <- case M.lookup tablename (dbTables db) of
    Nothing -> Left (TableNotFound tablename)
    Just t  -> return t
  when (length vals /= length (tableColumns table)) $
    Left (RowSizeMismatch (length (tableColumns table)) (length vals))
  -- constraints
  forM_ (zip (tableColumns table) vals) $ \(col, val) ->
    forM_ (constraints col) $ \constraint -> do
      let violated = case constraint of 
            NotNull -> val == Null
            Unique  -> elem val (values col) && val /= Null
      when violated $ Left (ConstraintViolated constraint (colName col))
    -- insert the vals!
  let table' :: Table
      table' = Table {
        tableColumns = zipWith insertIntoCol (tableColumns table) vals}
  return DB {
    dbTables = M.insert tablename table' (dbTables db)}
  
delete_ :: Text -- ^Table name
        -> [(Text, Predicate)]
        -> DB
        -> Either DBError DB
delete_ tname preds db = do
  table <- case M.lookup tname (dbTables db) of
    Nothing -> Left $ TableNotFound tname
    Just t -> return t
  let cols  = tableColumns table
  
  let cols' = cols {
        values = vals'}
      table' = Table {tableColumns = zipWith insertIntoCol (tableColumns table) vals'}
  return DB {dbTables = M.insert tname table' (dbTables db)}


main :: IO ()
main = return ()
