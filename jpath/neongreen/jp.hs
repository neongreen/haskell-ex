module Main where


-- General
import Data.Functor
import Data.Maybe
-- Text
import qualified Data.Text as T
import Data.Text (Text)
-- ByteString
import qualified Data.ByteString.Lazy.Char8 as BSL
-- JSON
import Data.Aeson                               -- from aeson
import Data.Aeson.Encode.Pretty                 -- from aeson-pretty
import qualified Data.HashMap.Strict as HM      -- from unordered-containers
import qualified Data.Vector as V               -- from vector
-- Parsing
import Text.Megaparsec                          -- from megaparsec
import Text.Megaparsec.Text
import Text.Megaparsec.Lexer (integer)
-- CLI stuff
import System.Environment (getArgs, getProgName)
import System.Exit (die)


----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

type Path = [Selector]

data Selector
  = Root          -- '$'
  | Field Text    -- '.foo'
  | Index Int     -- '[3]'

----------------------------------------------------------------------------
-- Parsing
----------------------------------------------------------------------------

pathP :: Parser Path
pathP = many selectorP

selectorP :: Parser Selector
selectorP = choice [
  string "$" $> Root,
  optional (char '.') *> (Field <$> fieldNameP),
  between (char '[') (char ']') (Index <$> indexP) ]

fieldNameP :: Parser Text
fieldNameP = label "field name" $
  T.pack <$> some (char '-' <|> char '_' <|> alphaNumChar)

indexP :: Parser Int
indexP = label "index" $
  fromInteger <$> integer

----------------------------------------------------------------------------
-- Selecting
----------------------------------------------------------------------------

select
  :: Selector  -- ^ Selector
  -> Value     -- ^ Root value (used if the selector is 'Root')
  -> Value     -- ^ Value to choose from
  -> [Value]
select  Root     root val          = [root]
select (Field k) root (Object obj) = maybeToList (HM.lookup k obj)
select (Index i) root (Array arr)  = maybeToList (arr V.!? i)
select _ _ _ = []

selectPath :: Path -> Value -> Value -> [Value]
selectPath [] _ val = [val]
selectPath (x:xs) root val =
  concatMap (selectPath xs root) (select x root val)

----------------------------------------------------------------------------
-- CLI, reading files, etc
----------------------------------------------------------------------------

printHelp :: IO ()
printHelp = do
  progName <- getProgName
  die $ unlines [
    "Usage: " ++ progName ++ " QUERY FILE",
    "",
    "Example: " ++ progName ++ " \"store.book[0]\" store.js" ]

processFile :: Text -> FilePath -> IO ()
processFile query file = do
  -- Parse the query as a path
  path <- case parse pathP "" query of
    Left err -> die $ "Error when parsing query: " ++ parseErrorPretty err
    Right path -> return path
  -- Parse JSON
  mbJson <- eitherDecode <$> BSL.readFile file
  json <- case mbJson of
    Left err -> die $ "Error when parsing JSON: " ++ err
    Right json -> return json
  -- Extract and print the results
  let results = selectPath path json json
  BSL.putStrLn $ encodePretty (Array (V.fromList results))

main :: IO ()
main = do
  args <- getArgs
  case args of
    [query, file] -> processFile (T.pack query) file
    _             -> printHelp
