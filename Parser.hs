module Parser (parseFile, parseString) where

import Ast

import Control.Applicative ((<*), (<$>))
import Text.Parsec hiding (parse)
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)


-- Exported functions

parseFile :: String -> IO (Either ParseError Program)
parseFile name = readFile name >>= return . parse name

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

type Parser = Parsec String (GUID, [(String, GUID)])

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

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

parseExpr :: Parser Expr
parseExpr = buildExpressionParser table term
            <?> "expression"
  where term = do i <- integer
                  uid <- guid
                  return $ EConst (fromInteger i) uid
               <|> try (do id <- ident
                           es <- parens $ commaSep parseExpr
                           uid <- guid
                           return $ EAppNamed id es uid)
               <|> try (do e <- parens parseExpr
                           es <- parens $ commaSep parseExpr
                           uid <- guid
                           return $ EAppUnnamed e es uid)
               <|> (ident >>= return . EVar)
               <|> parens parseExpr
               <|> (reserved "input" >> guid >>= return . EInput)
               <|> (reserved "malloc" >> guid >>= return . EMalloc)
               <|> (reserved "null" >> guid >>= return . ENull)
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
          where inleft op ast = Infix (reservedOp op >> guid >>= return . flip2 ast) AssocLeft
                pre op ast = Prefix (reservedOp op >> guid >>= return . flip ast)
                flip2 f c a b = f a b c

parseStm :: Parser Stm
parseStm = SSeq <$> (many1 stm1)
  where stm1 = (reserved "output" >> SOutput <$> parseExpr <* semi)
               <|> (reserved "var" >> SDecl <$> commaSep1 ident <* semi)
               <|> (reserved "return" >> SReturn <$> parseExpr <* semi)
               <|> do v <- ident
                      reservedOp "="
                      e <- parseExpr
                      semi
                      return $ SAss v e
               <|> do reservedOp "*"
                      v <- ident
                      reservedOp "="
                      e <- parseExpr
                      semi
                      return $ SAssRef v e
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

parseFunction :: Parser Function
parseFunction = do name <- ident
                   arguments <- parens $ commaSep ident
                   braces $ (FNamedSimple name arguments) <$> parseStm

parseProgram :: Parser Program
parseProgram = whiteSpace >> many parseFunction <* eof

parse :: String -> String -> Either ParseError Program
parse filename contents = runParser parseProgram (1, []) filename contents
