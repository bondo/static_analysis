module Parser (parseFile, parseString) where

import Ast

import Control.Applicative ((<*), (<$>), liftA2)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)


-- Exported functions

parseFile :: String -> IO (Either ParseError Program)
parseFile = parseFromFile parseProgram

parseString :: String -> Either ParseError Program
parseString = runParser parseProgram () "<insert filename here>"


-- The lexer

opLetters = "+-*/>=&"

def = emptyDef{ P.identStart = letter
              , P.identLetter = alphaNum
              , P.opStart  = oneOf opLetters
              , P.opLetter = oneOf opLetters
              , P.reservedOpNames = ["+", "-", "*", "/", ">", "==", "=", "&"]
              , P.reservedNames = ["input", "output", "if", "else", "while",
                                   "var", "return", "malloc", "null"]
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

parseExpr :: Parser Expr
parseExpr = buildExpressionParser table term
            <?> "expression"
  where term = (EConst . fromInteger) <$> integer
               <|> try (liftA2 EAppNamed identifier (parens $ commaSep parseExpr))
               <|> try (liftA2 EAppUnnamed (parens parseExpr) (parens $ commaSep parseExpr))
               <|> EVar <$> identifier
               <|> parens parseExpr
               <|> (reserved "input" >> return EInput)
               <|> (reserved "malloc" >> return EMalloc)
               <|> (reserved "null" >> return ENull)
               <|> (reservedOp "&" >> ERef <$> identifier)
               <?> "expression term"
        table = [ [pre "*" EDeRef]
                , [inleft "*" (EBinOp BMulti), inleft "/" (EBinOp BDiv)]
                , [inleft "+" (EBinOp BPlus), inleft "-" (EBinOp BMinus)]
                , [inleft ">" (EBinOp BGt), inleft "==" (EBinOp BEq)]
                ]
          where inleft op ast = Infix (reservedOp op >> return ast) AssocLeft
                pre op ast = Prefix (reservedOp op >> return ast)

parseStm :: Parser Stm
parseStm = SSeq <$> (many1 stm1)
  where stm1 = (reserved "output" >> SOutput <$> parseExpr <* semi)
               <|> (reserved "var" >> SDecl <$> commaSep1 identifier <* semi)
               <|> (reserved "return" >> SReturn <$> parseExpr <* semi)
               <|> do { v <- identifier
                      ; reservedOp "="
                      ; e <- parseExpr
                      ; semi
                      ; return (SAss v e)
                      }
               <|> do { reservedOp "*"
                      ; v <- identifier
                      ; reservedOp "="
                      ; e <- parseExpr
                      ; semi
                      ; return (SAssRef v e)
                      }
               <|> do { reserved "if"
                      ; e <- parens parseExpr
                      ; sif <- braces parseStm
                      ; stm <- do { reserved "else"
                                  ; selse <- braces parseStm
                                  ; return (SIfElse e sif selse)
                                  }
                               <|> return (SIf e sif)
                      ; return stm
                      }
               <|> do { reserved "while"
                      ; e <- parens parseExpr
                      ; s <- braces parseStm
                      ; return (SWhile e s)
                      }
               <?> "statement"

parseFunction :: Parser Function
parseFunction = do { name <- identifier
                   ; arguments <- parens $ commaSep identifier
                   ; braces $ (FNamed name arguments) <$> parseStm
                   }

parseProgram :: Parser Program
parseProgram = whiteSpace >> many parseFunction <* eof
