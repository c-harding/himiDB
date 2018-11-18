{-# language FlexibleContexts #-}
module Main where

import qualified Data.Database.DatabaseMonad as D
import Data.Database.DatabaseMonad (Database, empty)
import Data.Database.Types

import System.Console.Haskeline
import System.Directory (getHomeDirectory)
import Control.Monad.State.Strict
import System.Exit (exitSuccess)
import Control.Applicative (liftA2, optional, (<|>), (<**>))
import Data.Foldable (asum)
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Void
import qualified Data.List.NonEmpty as NE
import System.IO
import System.Console.ANSI
import Text.Megaparsec hiding (empty, getInput)
import Text.Megaparsec.Char hiding (space1)
import Control.Monad.Combinators.Expr

type Parser = Parsec Void String

data Input
  = Create String [Field] String
  | Insert String Record
  | Describe (Maybe String)
  | Drop String
  | Select String [String] Constraint
  | Delete String Constraint
  | Exit
  | Help
  deriving (Show)

space1 :: Parser ()
space1 = spaceChar *> hidden space

keyword :: String -> Parser String
keyword word = string word <* notFollowedBy alphaNumChar <* hidden space

keyword' :: String -> Parser String
keyword' word = string' word <* notFollowedBy alphaNumChar <* hidden space

symbol :: String -> Parser String
symbol sym = string sym <* hidden space

typeP :: Parser Type
typeP = IntRecord <$ keyword' "int"
    <|> StringRecord <$ keyword' "string"
   
nameP :: Parser String
nameP = liftA2 (:) letterChar (hidden $ many alphaNumChar) <* notFollowedBy alphaNumChar <* hidden space

fieldP :: Parser Field
fieldP = (,) <$> nameP <*> typeP

colsP :: Parser [String]
colsP = symbol "(" *> nameP `sepBy` (symbol ",") <* symbol ")"
    <|> [] <$ symbol "*"
    <|> return <$> nameP

fieldsP :: Parser [Field]
fieldsP = between (symbol "(") (symbol ")") $ fieldP `sepBy` symbol ","

createP :: Parser Input
createP = Create <$ keyword' "create" <*> nameP <*> fieldsP <* space <*> (many (noneOf "\n") <?> "description")

selectP :: Parser Input
selectP = Select <$ keyword' "select" <*> nameP <*> colsP <*> (keyword' "where" *> constraintP <|> pure All)

deleteP :: Parser Input
deleteP = Delete <$ keyword' "delete" <*> nameP <*> (keyword' "where" *> constraintP <|> pure All)

describeP :: Parser Input
describeP = Describe <$ keyword' "describe" <*> optional (space *> nameP)

dropP :: Parser Input
dropP = Drop <$ keyword' "drop" <*> nameP

intP :: Parser Int 
intP = read <$> liftA2 (++) (symbol "-" <|> pure "") (some digitChar) <* notFollowedBy digitChar

stringLitP :: Parser String
stringLitP = symbol "\"" *> many character <* char '"'
  where
    nonEscape = noneOf "\\\"\0\n"
    character = nonEscape <|> escape
    escape = do
      d <- char '\\'
      c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
      return $ read ['\'', d, c, '\'']

valueP :: Parser Value
valueP = IntValue <$> intP
     <|> StringValue <$> stringLitP

recordP :: Parser Record
recordP = between (symbol "(") (symbol ")") $ valueP `sepBy` symbol ","

insertP :: Parser Input
insertP = Insert <$ keyword' "insert" <*> nameP <*> recordP

inputP :: Parser Input
inputP = (<* eof) . (hidden space *>) . asum $ map try
  [ createP
  , insertP
  , describeP
  , dropP
  , selectP
  , Exit <$ keyword' "exit"
  , Help <$ keyword' "help"
  ]

constraintP :: Parser Constraint
constraintP = makeExprParser constrTermP constrOpP

constrOpP :: [[Operator Parser Constraint]]
constrOpP =
  [ [Prefix (Not <$ symbol "!") ]
  , [InfixL (And <$ symbol "&&")
    , InfixL (Or <$ symbol "||") ]
  ]

constrTermP :: Parser Constraint
constrTermP =  between (symbol "(") (symbol ")") constraintP
  <|> try intRelation <|> strRelation 

intRelation :: Parser Constraint
intRelation = (intExprP <**> intRelationOp) <*> intExprP

strRelation :: Parser Constraint
strRelation = (strExprP <**> strRelationOp) <*> strExprP

intRelationOp :: Parser (IntExpr -> IntExpr -> Constraint)  
intRelationOp = 
      IntEq <$ symbol "="
  <|> IntLt <$ symbol "<"
  <|> intGt <$ symbol ">"
  <|> intLe <$ symbol "<="
  <|> intGe <$ symbol ">="
  <|> intNe <$ symbol "<>"
  where
    intGt a b = IntLt b a
    intLe a b = Or (IntLt a b) (IntEq a b)
    intGe a b = Or (intGt a b) (IntEq a b)
    intNe a b = Or (IntLt a b) (IntLt b a)

strRelationOp :: Parser (StrExpr -> StrExpr -> Constraint) 
strRelationOp =
      StrEq <$ symbol "=="
  <|> strNe <$ symbol "!="
  where
    strNe a b = Not (StrEq a b)
   
intExprP :: Parser IntExpr
intExprP = Left <$> intP <|> Right <$> nameP

strExprP :: Parser StrExpr
strExprP = Left <$> stringLitP <|> Right <$> nameP

runInput :: (MonadState Database m, MonadIO m) => Input -> m () 
runInput input = case input of
  Create name pFields descr -> maybe (return ()) (liftIO . printError) =<< D.createTable name pFields descr
  Insert name record -> maybe (return ()) (liftIO . printError) =<< D.insertRecord name record
  Drop name -> maybe (return ()) (liftIO . printError) =<< D.deleteTable name
  Select name pFields contraints -> either (liftIO . printError) (liftIO . putStrLn . drawTable Nothing) =<< D.select name contraints pFields
  Delete name contraints -> maybe (return ()) (liftIO . printError) =<< D.deleteWhere name contraints
  Describe Nothing -> liftIO . putStrLn =<< D.showTables
  Describe (Just name) -> either (liftIO . printError) (liftIO . putStrLn) =<< D.describeTable name
  Exit -> liftIO exitSuccess
  Help -> liftIO $ putStrLn (helpMessage True)

getInput :: IO String
getInput = do
  settings <- haskelineSettings
  runInputT settings loop
  where
    loop = do
      handleInterrupt loop $ do 
        ms <- withInterrupt $ getInputLine "himiDB > "
        case ms of
          Just s
            | not (all isSpace s) -> return s
            | otherwise -> loop
          Nothing -> liftIO exitSuccess


repl :: (MonadState Database m, MonadIO m) => m () 
repl = forever $ do
  s <- liftIO getInput
  let eitherInput = parse inputP "" s
  case eitherInput of
    Left errMsg -> liftIO . printError . getParseError $ errMsg
    Right input -> runInput input
  
getParseError :: ParseErrorBundle String Void -> String
getParseError bundle
  | atStart = "Unrecognised command. Correct usage:\n\n" ++ helpMessage False
  | otherwise = intercalate "\n" . NE.toList . NE.map parseErrorTextPretty $ bundleErrors bundle
  where
    offset = errorOffset . NE.head . bundleErrors $ bundle
    atStart = all isSpace . take offset . pstateInput . bundlePosState $ bundle
    

printError :: String -> IO ()
printError msg = do
  setSGR [SetColor Foreground Vivid Red]
  hPutStrLn stderr ("Error: "++msg)
  setSGR []

main :: IO ()
main = do
  welcome
  evalStateT repl empty

welcome :: IO ()
welcome = mapM_ putStrLn
  [ "HimiDB v0.1"
  , "The Haskell In-Memory Interactive Database System"
  , "Created for the MuniHac 2018"
  , "type `help` for instructions"
  , ""
  ]

functions :: [(String, String, [String])]
functions =
  [ ("create", "Create a table", ["create myTable (a int, b int, c int)","create tableName (col1 int, col2 string) description goes here"])
  , ("drop", "Delete a table and its contents", ["drop tableName"])
  , ("describe", "Show all tables, or the data of one table", ["describe", "describe tableName"])
  , ("insert", "Insert a row into a table", ["insert tableName (1, \"me\")"])
  , ("select", "Select data from a table", ["select tableName *", "select tableName col1", "select tableName (col1, col2)"])
  , ("delete", "Delete data from a table", ["delete tableName"])
  , ("help", "Show the help guide, with examples", [])
  , ("exit", "Exit and clear the database", ["^D"])
  ]
helpMessage :: Bool -> String
helpMessage long = intercalate "\n" (header ++ (helpFunction =<< functions))
  where
    header = if long then ["Usage:",""] else []
    helpFunction (name, desc, ex)
      | long = [name++":", "  "++desc] ++ (("  > "++) <$> ex) ++ [""]
      | otherwise = [name++": "++desc]

haskelineSettings :: (Monad m, MonadIO io) => io (Settings m)
haskelineSettings = do
  getDir <- liftIO $ fmap (++ "/.himidb_history") getHomeDirectory
  return $ Settings noCompletion (Just getDir) True
