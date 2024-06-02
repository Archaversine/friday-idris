module Main 

import Data.String.Parser

import Friday.Parsing.Statement
import Friday.Parsing.Expression
import Friday.Optimize.Expression
import Friday.TypeChecker.Statement
import Friday.TypeChecker.Types

import Friday.Codegen.C

import System.File.ReadWrite

main : IO ()
main = readFile "test.fry" >>= \case
    Left err => putStrLn $ "File Error: " ++ show err
    Right xs => case parse parseProg xs of 
        Left err     => putStrLn $ "Parser Error: " ++ err
        Right (e, _) => case runChecker (readContext e >> checkTypes e) of 
            Left err => putStrLn $ "Type Error: " ++ err
            Right () => writeC "main.c" e >>= \case 
                Left err => putStrLn $ "Codegen error: " ++ show err
                Right () => putStrLn "Success!"

    --case parse parseExpression text of 
    --    Left err     => putStrLn $ "Error: " ++ err 
    --    Right (e, _) => do 
    --        printLn e
    --        printLn (reduceExpr e)

