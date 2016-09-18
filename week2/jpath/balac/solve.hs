{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-} 

import Control.Applicative (empty)
import Control.Monad (void)
import qualified Data.ByteString.Lazy as Lazy
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.Aeson.Types as AT
import Data.Functor
import qualified Data.HashMap.Strict as HM
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import Debug.Trace
import System.Environment
import System.Exit
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
rootOp = C.char '$' $> OpRoot

currentOp :: Parser JPOperator
currentOp = C.char '@' $> OpCurrent

childOp :: Parser JPOperator
childOp = OpChild <$> ( ( dot >> identifier ) <|> inSquares identifier <|> identifier )

allChildrenOp :: Parser JPOperator
allChildrenOp = ( C.char '*' <|> inSquares ( C.char '*' ) <|> ( dot >> C.char '*' ) ) $> OpAllChildren

parseSubscriptNum :: Parser [Integer]
parseSubscriptNum = return <$> inSquares integer

parseSubscriptUnion :: Parser [Integer]
parseSubscriptUnion = nub <$> inSquares ( sepBy1 integer semi )

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
parseOperators = ( lexeme $ some anyOp ) <* eof

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

getChildrenByIndices :: [Integer] -> Value -> AT.Parser ( V.Vector Value )
getChildrenByIndices indices = withArray "array" $ \arr -> do
                                    return $ V.fromList ( mapMaybe ( ( arr V.!? ) . fromInteger ) indices )

getAllChildren :: Value -> AT.Parser ( V.Vector Value )
getAllChildren (Object obj) = return $ V.fromList ( HM.elems obj )
getAllChildren (Array v)  = return v
getAllChildren _ = return V.empty

applyOperatorsToVec :: [JPOperator] -> V.Vector Value -> AT.Parser Value
applyOperatorsToVec ops childrenVec = Array <$> mapM ( applyJPOperator ops ) childrenVec

valueToVector :: Value -> V.Vector Value
valueToVector (Array v) = v
valueToVector v = V.singleton v

mFromMaybe :: ( Monoid m ) => Maybe m -> m
mFromMaybe Nothing   = mempty
mFromMaybe (Just x)  = x


applyOperatorsRecursively :: [JPOperator] -> Value -> AT.Parser Value
applyOperatorsRecursively ops value = do
    curResult <- optional( valueToVector <$> applyJPOperator ops value )
    rawChildResults <- getAllChildren value >>= mapM ( applyOperatorsRecursively ops )
    let flattened = V.concatMap valueToVector ( mFromMaybe curResult <> rawChildResults )
    let result = V.filter (/= Array V.empty) flattened
    return ( Array result )


applyJPOperator :: [JPOperator] -> Value -> AT.Parser Value
applyJPOperator [] value = return value
applyJPOperator (op:ops) value = case op of
    OpRoot                  -> applyJPOperator ops value
    OpCurrent               -> applyJPOperator ops value
    OpChild key             -> getChildByKey key value >>= applyJPOperator ops
    OpSubscriptSet indices  -> getChildrenByIndices indices value >>= applyOperatorsToVec ops
    OpAllChildren           -> getAllChildren value >>= applyOperatorsToVec ops
    OpRecursive             -> applyOperatorsRecursively ops value

getInputArgs :: IO (String,String)
getInputArgs = do
    args <- getArgs
    if length args < 2
        then do
            progName <- getProgName
            die $ "Usage: " ++ progName ++ " <jpath query> <json file>"
        else
            return ( head args, ( head . tail ) args )

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetBuffering stdout NoBuffering
    ( jpQuery, jsonFile ) <- getInputArgs
    let eJpath = runParser parseOperators "" jpQuery
    jsonValue <- readJsonFile jsonFile
    case eJpath of
        Left err -> putStr ( parseErrorPretty err )
        Right ops -> case AT.parseEither ( applyJPOperator ops ) jsonValue of
                        Left err -> putStrLn err
                        Right sel -> Lazy.putStr $ encodePretty sel