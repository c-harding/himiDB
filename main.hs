{-# language FlexibleContexts #-}
module Main where

import qualified Data.Database.DatabaseMonad as D
import Data.Database.DatabaseMonad (Database, empty)
import Data.Database.Types

import Control.Monad.State.Strict
import System.Exit (exitSuccess)
import Control.Applicative (liftA2, optional, (<|>))
import Data.Foldable (asum)
import Data.Char (isSpace)
import Data.List (intercalate)
import Data.Void
import qualified Data.List.NonEmpty as NE
import System.IO
import System.Console.ANSI
import Text.Megaparsec hiding (empty, getInput)
import Text.Megaparsec.Char hiding (space1)

type Parser = Parsec Void String

data Input
  = Create String [Field] String
  | Insert String Record
  | Describe (Maybe String)
  | Drop String
  | Select String Constraint [String]
  | Delete String Constraint
  | Exit
  | Help
  deriving (Show)

space1 :: Parser ()
space1 = spaceChar *> hidden space

tok :: String -> Parser String
tok word = hidden space *> string word

typeP :: Parser Type
typeP = IntRecord <$ tok "int" <|> StringRecord <$ tok "string"
   
nameP :: Parser String
nameP = hidden space *> liftA2 (:) letterChar (hidden $ many alphaNumChar)

fieldP :: Parser Field
fieldP = (,) <$> nameP <* space1 <*> typeP

fieldsP :: Parser [Field]
fieldsP = tok "(" *> fieldP `sepBy` (tok ",") <* tok ")"

createP :: Parser Input
createP = Create <$ tok "create" <* space1 <*> nameP <*> fieldsP <* space <*> (many (noneOf "\n") <?> "description")

describeP :: Parser Input
describeP = Describe <$ tok "describe" <*> optional (space *> nameP)

dropP :: Parser Input
dropP = Drop <$ tok "drop" <* space1 <*> nameP

intP :: Parser Int 
intP = read <$> liftA2 (++) (string "-" <|> pure "") (some digitChar)

parseString :: Parser String
parseString = tok "\"" *> many character <* char '"'
  where
    nonEscape = noneOf "\\\"\0\n"
    character = nonEscape <|> escape
    escape = do
      d <- char '\\'
      c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
      return $ read ['\'', d, c, '\'']

valueP :: Parser Value
valueP = IntValue <$> intP
     <|> StringValue <$> parseString

recordP :: Parser Record
recordP = tok "(" *> (hidden space *> valueP) `sepBy` (tok ",") <* tok ")"

insertP :: Parser Input
insertP = Insert <$ tok "insert" <* space1 <*> nameP <* space1 <*> recordP

inputP :: Parser Input
inputP = asum $ map try
  [ createP
  , insertP
  , describeP
  , dropP
  , Exit <$ tok "exit"
  , Help <$ tok "help"
  ]

runInput :: (MonadState Database m, MonadIO m) => Input -> m () 
runInput input = case input of
  Create name pFields descr -> D.createTable name pFields descr >> get >>= liftIO . print
  Insert name record -> maybe (return ()) (liftIO . printError) =<< D.insertRecord name record
  Drop name -> maybe (return ()) (liftIO . printError) =<< D.deleteTable name
  Select name contraints pFields -> either (liftIO . printError) (liftIO . print) =<< D.select name contraints pFields
  Delete name contraints -> maybe (return ()) (liftIO . printError) =<< D.deleteWhere name contraints
  Describe Nothing -> liftIO . putStrLn =<< D.showTables
  Describe (Just name) -> either (liftIO . printError) (liftIO . putStrLn) =<< D.describeTable name
  Exit -> liftIO exitSuccess
  Help -> liftIO $ putStrLn (helpMessage True)

getInput :: IO String
getInput = do
  putStr "himiDB > "
  s <- getLine
  if all isSpace s then getInput else return s

repl :: (MonadState Database m, MonadIO m) => m () 
repl = [1..] `forM_` \i -> do
  s <- liftIO getInput
  let eitherInput = parse inputP ("<repl>:"++show (i :: Int)) s
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
  , ("describe", "Describe a table, or all tables", ["describe", "describe tableName"])
  , ("insert", "Insert a row into a table", ["insert tableName (1, \"me\")"])
  , ("help", "Show the help guide, with examples", [])
  , ("exit", "Exit and clear the database", ["^C"])
  ]
helpMessage :: Bool -> String
helpMessage long = intercalate "\n" (header ++ (helpFunction =<< functions))
  where
    header = if long then ["Usage:",""] else []
    helpFunction (name, desc, ex)
      | long = [name++":", "  "++desc] ++ (("  > "++) <$> ex) ++ [""]
      | otherwise = [name++": "++desc]