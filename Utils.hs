module Utils where

import Ast (Program, showProgram)
import Constraints (Constraint, showConstraint, showConstraints, generateConstraints)
import DisjointSet (string, result)
import Parser (parseString, parseFile)
import UnifyTypes (unify, unifyAll, DS_TV)
import Weeder (weed)

import Data.List (intercalate, intersperse)

parseAndWeedString :: String -> Either String Program
parseAndWeedString = either (Left . show) weed . parseString

parseAndWeedFile :: String -> IO (Either String Program)
parseAndWeedFile fname = parseFile fname >>= return . either (Left . show) weed

printEither :: Either String Program -> IO ()
printEither esp = case esp of
  Left e -> putStrLn $ "Failed: " ++ e
  Right p -> putStrLn $ showProgram p

printParsedString :: String -> IO ()
printParsedString = printEither . parseAndWeedString

printParsedFile :: String -> IO ()
printParsedFile fname = parseAndWeedFile fname >>= printEither

getRight :: Show a => Either a b -> b
getRight = either (error . show) id

getProgramFromString :: String -> Program
getProgramFromString str =  getRight $ parseAndWeedString str

getProgramFromFile :: String -> IO Program
getProgramFromFile fname = parseAndWeedFile fname >>= return . getRight

getConstraingsFromFile :: String -> IO [Constraint]
getConstraingsFromFile fname = getProgramFromFile fname >>= return . generateConstraints

printConstraintsFromString :: String -> IO ()
printConstraintsFromString str = do
  let program = getProgramFromString str
  putStrLn $ concat (map ((++"\n\n") . show) program)
  putStrLn . showConstraints . generateConstraints $ program

printConstraintsFromFile :: String -> IO ()
printConstraintsFromFile fname = readFile fname >>= printConstraintsFromString

printUnifyFromFile :: String -> IO ()
printUnifyFromFile fname = do
  constraints <- getConstraingsFromFile fname
  putStrLn . result $ unifyAll constraints >> string

verboseUnifyFromFile :: String -> IO ()
verboseUnifyFromFile fname = do cs <- getConstraingsFromFile fname
                                mapM_ (putStrLn . (++"\n")) . result . unifyAllVerbose $ cs
  where unifyAllVerbose :: [Constraint] -> DS_TV [String]
        unifyAllVerbose = mapM unifyVerbose
        unifyVerbose :: Constraint -> DS_TV String
        unifyVerbose c = unify c >> string >>= return . (cont c ++)
        cont c = "Constraint: " ++ showConstraint c ++ "\n"
