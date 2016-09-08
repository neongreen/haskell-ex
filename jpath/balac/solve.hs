{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (empty)
import Control.Monad (void)
import qualified Data.ByteString.Lazy as Lazy
import Data.Aeson
import qualified Data.Aeson.Types as AT
import Data.Maybe
import qualified Data.Set as S
import System.IO
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Lexer as L


data JPOperator = OpRoot 
                | OpCurrent
                | OpChild String
                | OpRecursive JPOperator
                | OpAll
                | OpSubscriptSet ( S.Set Integer )
                deriving( Show, Eq )

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

symbol :: String -> Parser String
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Integer
integer = lexeme L.integer

squares :: Parser a -> Parser a
squares = between (symbol "[") (symbol "]")

dot :: Parser Char
dot = C.char '.'

root :: Parser Char
root = C.char '$'

current :: Parser Char
current = C.char '@'

identifier :: Parser String
identifier = lexeme $ (:) <$> C.letterChar <*> many C.alphaNumChar

rootOp :: Parser JPOperator
rootOp = root >> return OpRoot

currentOp :: Parser JPOperator
currentOp = current >> return OpCurrent

anyOp :: Parser JPOperator
anyOp = rootOp <|> currentOp

parseOperators :: Parser [JPOperator]
parseOperators = lexeme $ some anyOp

readJsonFile :: FilePath -> IO Value
readJsonFile filePath = do
    fileData <- Lazy.readFile filePath
    let jsonValue = decode fileData :: Maybe Value
    case jsonValue of
        Nothing -> do
            print $ "Error parsing json file: " ++ filePath
            putStr "Enter new file name: "
            fileName <- getLine
            readJsonFile fileName
        Just val -> return val

main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering
    jsonValue <- readJsonFile "store.js"
    print jsonValue


