{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (empty)
import Control.Monad (void)
import qualified Data.ByteString.Lazy as Lazy
import Data.Aeson
import qualified Data.Aeson.Types as AT
import Data.List
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
                | OpRecursive
                | OpAllChildren
                | OpSubscriptSet [Integer]
                deriving( Show, Eq )

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

symbol :: String -> Parser String
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Integer
integer = lexeme L.integer

inSquares :: Parser a -> Parser a
inSquares = between (symbol "[") (symbol "]")

dot :: Parser Char
dot = C.char '.'

semi :: Parser Char
semi = C.char ','

colon :: Parser Char
colon = C.char ':'

identifier :: Parser String
identifier = lexeme $ (:) <$> C.letterChar <*> many C.alphaNumChar

rootOp :: Parser JPOperator
rootOp = const OpRoot <$> C.char '$'

currentOp :: Parser JPOperator
currentOp = const OpCurrent <$> C.char '@'

childOp :: Parser JPOperator
childOp = OpChild <$> ( ( dot >> identifier ) <|> inSquares identifier <|> identifier )

allChildrenOp :: Parser JPOperator
allChildrenOp = const OpAllChildren <$> ( C.char '*' <|> inSquares ( C.char '*' ) <|> ( dot >> C.char '*' ) )

parseSubscriptNum :: Parser [Integer]
parseSubscriptNum = return <$> inSquares integer

parseSubscriptUnion :: Parser [Integer]
parseSubscriptUnion = nub <$> ( inSquares $ sepBy1 integer semi )

firstNIndices :: Integer -> [Integer]
firstNIndices n = [0 .. n-1]

indicesRange :: Parser [Integer]
indicesRange = do
    fromIndex <- integer
    _ <- colon
    toIndex <- integer
    return [ fromIndex .. toIndex ]

parseSubscriptSlice :: Parser [Integer]
parseSubscriptSlice = inSquares ( ( firstNIndices <$> ( colon >> integer ) ) <|> indicesRange ) 

subscriptSetOp :: Parser JPOperator
subscriptSetOp = OpSubscriptSet <$> ( try parseSubscriptNum <|> try parseSubscriptUnion <|> try parseSubscriptSlice )

recursiveOp :: Parser JPOperator
recursiveOp = const OpRecursive <$> ( dot >> dot )

anyOp :: Parser JPOperator
anyOp = rootOp <|> currentOp <|> try allChildrenOp <|> try childOp <|> try subscriptSetOp <|> try recursiveOp

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


