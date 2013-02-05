module Utils where

import Ast
import Parser
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
