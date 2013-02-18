{-# LANGUAGE BangPatterns #-}

module Utils where

import Ast (Program, showProgram)
import CheckScopes (checkScopes)
import Constraints (Constraint, showConstraint, showConstraints, generateConstraints)
import DisjointSet (string, result)
import Parser (parseString, parseFile)
import UnifyTypes (unify, unifyAll, getTypes, DS_TV)
import Weeder (weed)

import Control.DeepSeq (force)
import Control.Monad (liftM, forM_)
import Data.List (intercalate, intersperse)

parseAndWeedString :: String -> Either String Program
parseAndWeedString = either (Left . show) weed . parseString

parseAndWeedFile :: String -> IO (Either String Program)
parseAndWeedFile fname = either (Left . show) weed `liftM` parseFile fname

printEither :: Either String Program -> IO ()
printEither = putStrLn . either ("Failed: " ++) showProgram

printParsedString :: String -> IO ()
printParsedString = printEither . parseAndWeedString

printParsedFile :: String -> IO ()
printParsedFile fname = parseAndWeedFile fname >>= printEither

getRight :: Show a => Either a b -> b
getRight = either (error . show) id

getProgramFromString :: String -> Program
getProgramFromString str =  getRight $ parseAndWeedString str

getProgramFromFile :: String -> IO Program
getProgramFromFile fname = getRight `liftM` parseAndWeedFile fname

getConstraingsFromFile :: String -> IO [Constraint]
getConstraingsFromFile fname = do program <- getProgramFromFile fname
                                  let !ids = checkScopes program
                                  return $ generateConstraints program

printConstraintsFromString :: String -> IO ()
printConstraintsFromString str = do
  let program = getProgramFromString str
  let !ids = checkScopes program
  putStrLn $ concatMap ((++"\n\n") . show) program
  putStrLn . showConstraints . generateConstraints $ program

printConstraintsFromFile :: String -> IO ()
printConstraintsFromFile fname = readFile fname >>= printConstraintsFromString

printUnifyFromFile :: String -> IO ()
printUnifyFromFile fname = getConstraingsFromFile fname >>= print . unifyAll

verboseUnifyFromFile :: String -> IO ()
verboseUnifyFromFile fname = do cs <- getConstraingsFromFile fname
                                mapM_ (putStrLn . (++"\n")) . result . unifyAllVerbose $ cs
  where unifyAllVerbose :: [Constraint] -> DS_TV [String]
        unifyAllVerbose = mapM unifyVerbose
        unifyVerbose :: Constraint -> DS_TV String
        unifyVerbose c = (cont c ++) `liftM` (unify c >> string)
        cont c = "Constraint: " ++ showConstraint c ++ "\n"

printTypesFromFile :: String -> IO ()
printTypesFromFile fname = do
  cont <- getConstraingsFromFile fname
  forM_ (force $ getTypes cont) $ \(e, t) -> putStrLn $ "[" ++ show e ++ "] = " ++ show t

verboseTypesFromFile :: String -> IO ()
verboseTypesFromFile fname = do
  putStrLn "Program:"     >> printParsedFile fname    >> putStrLn "\n"
  putStrLn "Eq. classes:" >> printUnifyFromFile fname >> putStrLn "\n"
  putStrLn "Types:"       >> printTypesFromFile fname
