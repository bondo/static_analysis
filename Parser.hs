module Parser (parseFile, parseString) where

import Ast (Program, Function(..), Stm(..), Expr(..), BinOp(..), Id(..), GUID)

import Control.Applicative ((<*), (<$>))
import Control.Monad (liftM)
import Text.Parsec hiding (parse)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Prelude hiding (fst, snd)


-- Exported functions

parseFile :: String -> IO (Either ParseError Program)
parseFile name = parse name `liftM` readFile name

parseString :: String -> Either ParseError Program
parseString = parse "<insert filename here>"


-- The lexer

opLetters = "+-*/>=&"

def = emptyDef{ P.identStart      = letter
              , P.identLetter     = alphaNum
              , P.opStart         = oneOf opLetters
              , P.opLetter        = oneOf opLetters
              , P.reservedOpNames = ["+", "-", "*", "/", ">", "==", "=", "&"]
              , P.reservedNames   = ["input", "output", "if", "else", "while",
                                     "var", "return", "malloc", "null"]
              , P.commentLine     = "//"
              , P.commentStart    = "/*"
              , P.commentEnd      = "*/"
              , P.nestedComments  = False
              }

P.TokenParser{ P.parens     = parens
             , P.braces     = braces
             , P.semi       = semi
             , P.identifier = identifier
             , P.integer    = integer
             , P.reservedOp = reservedOp
             , P.reserved   = reserved
             , P.semiSep1   = semiSep1
             , P.commaSep   = commaSep
             , P.commaSep1  = commaSep1
             , P.whiteSpace = whiteSpace } = P.makeTokenParser def


-- The parser

type Parser = Parsec String (GUID, [(String, GUID)], [(Int, GUID)])

fst :: (a, b, c) -> a
fst (a, b, c) = a

snd :: (a, b, c) -> b
snd (a, b, c) = b

trd :: (a, b, c) -> c
trd (a, b, c) = c

mapFst :: (a -> d) -> (a, b, c) -> (d, b, c)
mapFst f (a, b, c) = (f a, b, c)

mapSnd :: (b -> d) -> (a, b, c) -> (a, d, c)
mapSnd f (a, b, c) = (a, f b, c)

mapTrd :: (c -> d) -> (a, b, c) -> (a, b, d)
mapTrd f (a, b, c) = (a, b, f c)

guid :: Parser GUID
guid = do uid <- fst <$> getState
          modifyState $ mapFst (+1)
          return uid

ident :: Parser Id
ident = do id <- identifier
           vars <- snd <$> getState
           maybe (new id) (return . Id id) $ lookup id vars
  where new id = do uid <- guid
                    modifyState $ mapSnd ((id, uid):)
                    return $ Id id uid

int :: Parser Expr
int = do i <- integer
         is <- trd <$> getState
         let ii = fromInteger i
         maybe (new ii) (return . EConst ii) $ lookup ii is
  where new i = do uid <- guid
                   modifyState $ mapTrd ((i, uid):)
                   return $ EConst i uid

parseExpr :: Parser Expr
parseExpr = buildExpressionParser table term
            <?> "expression"
  where term = int
               <|> try (do id <- ident
                           es <- parens $ commaSep parseExpr
                           uid <- guid
                           return $ EAppNamed id es uid)
               <|> try (do e <- parens parseExpr
                           es <- parens $ commaSep parseExpr
                           uid <- guid
                           return $ EAppUnnamed e es uid)
               <|> EVar `liftM` ident
               <|> parens parseExpr
               <|> EInput `liftM` (reserved "input" >> guid)
               <|> EMalloc `liftM` (reserved "malloc" >> guid)
               <|> ENull `liftM` (reserved "null" >> guid)
               <|> do reservedOp "&"
                      id <- ident
                      uid <- guid
                      return $ ERef id uid
               <?> "expression term"
        table = [ [pre "*" EDeRef]
                , [inleft "*" (EBinOp BMulti), inleft "/" (EBinOp BDiv)]
                , [inleft "+" (EBinOp BPlus), inleft "-" (EBinOp BMinus)]
                , [inleft ">" (EBinOp BGt), inleft "==" (EBinOp BEq)]
                ]
          where inleft op ast = Infix (flip2 ast `liftM` (reservedOp op >> guid)) AssocLeft
                pre op ast = Prefix (flip ast `liftM` (reservedOp op >> guid))
                flip2 f c a b = f a b c

parseStm :: Parser Stm
parseStm = SSeq <$> many1 stm1
  where stm1 = (reserved "output" >> SOutput <$> parseExpr <* semi)
               <|> (reserved "var" >> SDecl <$> commaSep1 ident <* semi)
               <|> (reserved "return" >> SReturn <$> parseExpr <* semi)
               <|> parseAss SAss
               <|> (reservedOp "*" >> parseAss SAssRef)
               <|> do reserved "if"
                      e <- parens parseExpr
                      sif <- braces parseStm
                      selse <- (reserved "else" >> braces parseStm) <|> return SNop
                      return $ SIfElse e sif selse
               <|> do reserved "while"
                      e <- parens parseExpr
                      s <- braces parseStm
                      return $ SWhile e s
               <?> "statement"

parseAss :: (Id -> Expr -> Stm) -> Parser Stm
parseAss ass = do v <- ident
                  reservedOp "="
                  e <- parseExpr
                  semi
                  return $ ass v e

parseFunction :: Parser Function
parseFunction = do name <- ident
                   arguments <- parens $ commaSep ident
                   braces $ FNamedSimple name arguments <$> parseStm

parseProgram :: Parser Program
parseProgram = whiteSpace >> many parseFunction <* eof

parse :: String -> String -> Either ParseError Program
parse = runParser parseProgram (1, [], [])
