module Ast where

type Id = String

data Expr = EConst Int
          | EVar Id
          | EBinOp BinOp Expr Expr
          | EInput
          | EAppNamed Id [Expr] -- Function name and list of arguments
          | EAppUnnamed Expr [Expr] -- (E)(E,...,E)
          | ERef Id -- &id
          | EMalloc
          | EDeRef Expr -- *E
          | ENull
          deriving (Show)

data BinOp = BPlus
           | BMinus
           | BMulti
           | BDiv
           | BGt
           | BEq
           deriving (Show)

data Stm = SAss Id Expr
         | SAssRef Id Expr -- *id = E
         | SOutput Expr
         | SSeq [Stm]
         | SIf Expr Stm
         | SIfElse Expr Stm Stm
         | SWhile Expr Stm
         | SDecl [Id]
         | SReturn Expr
         | SNop
         deriving (Show)

data Function = FNamed Id [Id] Stm -- Name, arguments, body
              | FNamedReturn Id [Id] Stm Expr -- Name, arguments, body, return
              deriving (Show)
                  
type Program = [Function]
