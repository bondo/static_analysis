module Utils where

import Ast
import Constraints
import Parser
import TypeVariable
import Weeder

parseAndWeedString :: String -> Either String Program
parseAndWeedString = either (Left . show) weed . parseString

parseAndWeedFile :: String -> IO (Either String Program)
parseAndWeedFile fname = parseFile fname >>= return . either (Left . show) weed

printEither :: Either String Program -> IO ()
printEither esp = case esp of
  Left e -> error e
  Right p -> putStrLn $ showProgram p

printParsedString :: String -> IO ()
printParsedString = printEither . parseAndWeedString

printParsedFile :: String -> IO ()
printParsedFile fname = parseAndWeedFile fname >>= printEither

getRight :: Show a => Either a b -> b
getRight = either (error . show) id

getProgramFromFile :: String -> IO Program
getProgramFromFile fname = parseAndWeedFile fname >>= return . getRight

getConstraingsFromFile :: String -> IO [Constraint]
getConstraingsFromFile fname = getProgramFromFile fname >>= return . generateConstraints

printConstraintsFromFile :: String -> IO ()
printConstraintsFromFile fname = do
  program <- getProgramFromFile fname
  putStrLn $ concat (map ((++"\n\n") . show) program)
  putStrLn . showConstraints . generateConstraints $ program
