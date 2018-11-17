module Main where

import Data.Database.Database
import Data.Database.Record
import Data.Database.Table
import Control.Monad.State.Strict
import System.Exit (exitSuccess)
import Data.List (foldl')
import Control.Monad

-- parsing stuff
import Text.Parsec
import Text.Parsec.Token

main :: IO ()
main = return ()
--main = evalStateT getInput []

--g :: [String] -> StateT Database IO ()
--g wrds@(cmd:arguments) = foldl' f wrds commands
--
--f :: [String] -> [(String, String, Int, [String] -> StateT Database IO ())] -> StateT Database IO ()
--f acc b = undefined
--
--commands :: [(String, String, Int, [String] -> StateT Database IO ())]
--commands =
--    [ ("create", "Create Table", 2, 
--        \arguments -> 
--            do
--                let (name:fields:[])= arguments
--                --modify $ createTable name fields
--                modify $ createTable name [("asdf", IntRecord)]
--)]
    --,("describe", "Describe Table",)
    --,("insert", "Insert Record", --    ),("delete", "Delete Record",)
    --,("select", "Select Record",)
    --,("exit", "Exit",)

typeP :: Parsec String () Type
typeP = do
    s <- many1 alphaNum
    case s of
        "IntRecord"    -> return IntRecord
        "StringRecord" -> return StringRecord
        _              -> fail "String doesn't fit Type" 
    
fieldP :: Parsec String () Field
fieldP = do
    char '('
    s <- many1 alphaNum
    char ','
    t <- typeP
    char ')'
    return (s,t)

fieldsP :: Parsec String () [Field]
fieldsP = do
    char '['
    fields <- fieldP `sepBy` (char ',')
    char ']'
    return fields

getInput :: StateT Database IO ()
getInput = do
    liftIO $ putStrLn "Enter a database command or 'exit' to quit."
    s <- liftIO $ getLine
    let (cmd:arguments) = words s
    when (hasCorrectNumberOfArguments cmd arguments) $ do
        case cmd of
            "create"   -> do
                            let (name:fields:[])= arguments
                                result = parse fieldsP "parseFields" fields
                            case result of
                                Left errMsg   -> pure ()
                                Right fields' -> modify $ createTable name fields'
--            "insert"   -> do
--                            let (name:record:[]) = arguments
--                            db <- get 
--                            let mdb = insertRecord name record db
--                            case mdb of
--                                Nothing -> return ()
--                                Just newDB -> put newDB
--            "describe" -> do
--                            let (name:[]) = arguments
--                            db <- get
--                            let mdb = describeTable name db
--                            case mdb of
--                                Nothing -> liftIO $ putStrLn "Table doesn't exist"
--                                Just response -> liftIO $ putStrLn response 
--            "delete"   -> do
--                            let (name:[]) = arguments
--                            db <- get
--                            let mdb = deleteTable name db
--                            case mdb of
--                                Nothing -> return ()
--                                Just newDB -> put newDB 
--            "update"   -> do
--                            let (name:fields:[]) = arguments
--                            db <- get
--                            let mdb = deleteTable name db
--                            case mdb of
--                                Nothing -> return ()
--                                Just newDB -> modify $ createTable name fields newDB
--            "select"   -> do
--                            let (name:constraints:outputs) = arguments
--                            db <- get
--                            let mls = select name constraints outputs db
--                            case mls of
--                                Nothing   -> liftIO $ putStrLn "Nothing found."
--                                Just   ls -> mapM_ print ls
--            "exit"     -> liftIO $ exitSuccess
--            _          -> return ()

-- counting the number of arguments, disregarding the Database argument
hasCorrectNumberOfArguments :: String -> [String] -> Bool
hasCorrectNumberOfArguments cmd arguments = 
    let noExpectedArgs = case cmd of
                            "describe" -> 1
                            "delete"   -> 1
                            "create"   -> 2
                            "insert"   -> 2
                            "update"   -> 2
                            "select"   -> 3
                            _          -> 0
    in noExpectedArgs == length arguments
