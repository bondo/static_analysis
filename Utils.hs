module Utils where

import Ast
import Parser
import Weeder

parseAndWeedString :: String -> Either String Program
parseAndWeedString = either (Left . show) weed . parseString

parseAndWeedFile :: String -> IO (Either String Program)
parseAndWeedFile fname = parseFile fname >>= return . either (Left . show) weed