module Main where

import Data.Database.Database
import Control.Monad.State.Strict
import System.Exit (exitSuccess)

main :: IO ()
main = return ()
--main = evalStateT getInput []

--commands :: [(String, String, Int, StateT Database IO ())]
--commands =
--    [ ("create", "Create Table", 2, 
--        do
--            let (name:fields:[])= arguments
--            modify $ createTable name fields
--	)]
    --,("describe", "Describe Table",)
    --,("insert", "Insert Record", --    ),("delete", "Delete Record",)
    --,("select", "Select Record",)
    --,("exit", "Exit",)

getInput :: StateT Database IO ()
getInput = do
    liftIO $ putStrLn "Enter a database command or 'exit' to quit."
    s <- liftIO $ getLine
    let (cmd:arguments) = words s
    when (hasCorrectNumberOfArguments cmd arguments) $ do
        case cmd of
            "create"   -> do
                            let (name:fields:[])= arguments
                            modify $ createTable name fields
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
