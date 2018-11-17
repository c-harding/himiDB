{-# language FlexibleContexts #-}
module Main where

import Control.Monad.State.Strict
import System.Exit (exitSuccess)
import Control.Applicative (some, liftA2)
import System.IO

-- parsing stuff
import Text.Parsec hiding (getInput)

import qualified Data.Database.DatabaseMonad as DBM
import Data.Database.Database
import Data.Database.Types
import Data.Database.Table


type Parser = Parsec String ()

data Input
  = Create String [Field] String
  | Insert String Record
  | Describe String
  | Delete String
  | Select String Constraint [String]
  | Exit
  deriving (Show)

tok :: String -> Parser String
tok word = spaces *> string word

typeP :: Parser Type
typeP = 
    IntRecord <$ tok "int" <|> StringRecord <$ tok "string"
   
nameP :: Parser String
nameP = liftA2 (:) letter (many alphaNum)

fieldP :: Parser Field
fieldP = (,) <$ spaces <*> nameP <* space <*> typeP

fieldsP :: Parser [Field]
fieldsP = tok "(" *> fieldP `sepBy` (tok ",") <* tok ")"

createP :: Parser Input
createP = 
    Create <$ tok "create" <* some space <*> nameP <*> fieldsP <* some space <*> many anyChar
-- <*> many alphaNum

describeP :: Parser Input
describeP = 
    Describe <$ tok "describe" <* some space <*> nameP

deleteP :: Parser Input
deleteP = 
    Delete <$ tok "delete" <* some space <*> nameP

intP :: Parser Int 
intP = read <$> some digit

valueP :: Parser Value
valueP = 
        IntValue <$> intP 
    <|> StringValue <$ tok "\"" <*> some alphaNum <* char '"'

recordP :: Parser Record
recordP = 
    tok "(" *> valueP `sepBy` (tok ",") <* tok ")"

insertP :: Parser Input
insertP = 
    Insert <$ tok "insert" <* some space <*> nameP <* some space <*> recordP

exitP :: Parser Input
exitP = Exit <$ string "exit"

inputP :: Parser Input
inputP =
        createP
    <|> insertP
    <|> try describeP
    <|> deleteP
    <|> exitP

input2State :: (MonadState Database m, MonadIO m) => Input -> m () 
input2State (Create name pFields descr) = 
    DBM.createTable name pFields descr >> get >>= liftIO . print
input2State (Insert name record) = 
    DBM.insertRecord name record >> get >>= liftIO . print
input2State (Delete name)        = 
   DBM.deleteTable name >> get >>= liftIO . print
input2State (Select name contraints pFields) =
    either (liftIO . hPutStrLn stderr . ("Error: "++)) (liftIO . print) =<< DBM.select name contraints pFields
input2State (Describe name) = 
    either (liftIO . hPutStrLn stderr . ("Error: "++)) (liftIO . putStrLn) =<< DBM.describeTable name
input2State Exit            = liftIO exitSuccess

getInput :: (MonadState Database m, MonadIO m) => m () 
getInput = forever $ do
    s <- liftIO getLine
    let eitherInput = parse inputP "" s
    case eitherInput of
        Left errMsg -> liftIO . hPutStrLn stderr . show $ errMsg
        Right input -> input2State input

main :: IO ()
main = evalStateT getInput []
