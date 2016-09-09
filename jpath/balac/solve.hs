{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (empty)
import Control.Monad (void)
import qualified Data.ByteString.Lazy as Lazy
import Data.Aeson
import qualified Data.Aeson.Types as AT
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Environment
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


getChildByKey :: String -> Value -> AT.Parser Value
getChildByKey key = withObject "object" $ \obj ->
                        obj .: T.pack key

getChildrenByIndices :: [Integer] -> Value -> AT.Parser Value
getChildrenByIndices indices = withArray "array" $ \arr -> do
                                    let maybeChildren = map ( ( arr V.!? ) . fromInteger ) indices
                                    let children = map fromJust $ filter isJust maybeChildren
                                    return $ Array ( V.fromList children )

getAllChildren :: Value -> AT.Parser Value
getAllChildren (Object obj) = return $ Array ( V.fromList ( HM.elems obj ) )

applyOperatorsToArray :: [JPOperator] -> Value -> AT.Parser Value
applyOperatorsToArray ops ( Array childrenVec ) = do
    results <- mapM ( applyJPOperator ops ) childrenVec
    return $ Array results 

applyJPOperator :: [JPOperator] -> Value -> AT.Parser Value
applyJPOperator [] value = return value
applyJPOperator (op:ops) value = case op of
    OpRoot                  -> applyJPOperator ops value
    OpCurrent               -> applyJPOperator ops value
    OpChild key             -> getChildByKey key value >>= applyJPOperator ops
    OpSubscriptSet indices  -> getChildrenByIndices indices value >>= applyOperatorsToArray ops
    OpAllChildren           -> getAllChildren value >>= applyOperatorsToArray ops

printValue :: Value -> IO ()
printValue = undefined

main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering
    jsonValue <- readJsonFile "store.js"
    args <- getArgs
    mapM putStrLn args
    let eJpath = runParser parseOperators "" $ head args
    case eJpath of
        Left err -> putStr ( parseErrorPretty err )
        Right ops -> do
            case AT.parseEither ( applyJPOperator ops ) jsonValue of
                Left err -> print $ "Error: " ++ err
                Right sel -> print sel