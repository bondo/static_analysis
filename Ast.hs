module Ast where

type Id = String

data Expr = EConst      { e_val  :: Int,   e_uid :: Int }
          | EVar        { e_name :: Id,    e_uid :: Int }
          | EBinOp      { e_op   :: BinOp, e_left :: Expr,   e_right :: Expr, e_uid :: Int }
          | EAppNamed   { e_name :: Id,    e_args :: [Expr], e_uid :: Int }
          | EAppUnnamed { e_expr :: Expr,  e_args :: [Expr], e_uid :: Int } -- (E)(E,...,E)
          | ERef        { e_name :: Id,    e_uid :: Int } -- &id
          | EDeRef      { e_expr :: Expr,  e_uid :: Int } -- *E
          | EInput      { e_uid  :: Int }
          | EMalloc     { e_uid  :: Int }
          | ENull       { e_uid  :: Int }
          deriving (Show)

instance Eq Expr where
  a == b = e_uid a == e_uid b

data BinOp = BPlus
           | BMinus
           | BMulti
           | BDiv
           | BGt
           | BEq
           deriving (Show, Eq)

data Stm = SAss    { s_name :: Id, s_val :: Expr }
         | SAssRef { s_name :: Id, s_val :: Expr } -- *id = E
         | SOutput { s_val  :: Expr }
         | SSeq    { s_stms :: [Stm] }
--       | SIf     { s_cond :: Expr, s_then :: Stm } -- Just use SNop in the else branch
         | SIfElse { s_cond :: Expr, s_then :: Stm, s_else :: Stm }
         | SWhile  { s_cond :: Expr, s_body :: Stm }
         | SDecl   { s_ids  :: [Id] }
         | SReturn { s_val  :: Expr } -- Only use this to simplify passing, weed it out later
         | SNop -- Has no syntax
         deriving (Show)

data Function =
  -- Use when there's SReturns in Stm, convert to FNamed in the weeder
     FNamedSimple { f_name :: Id, f_formals :: [Id], f_body :: Stm }
   | FNamed       { f_name :: Id, f_formals :: [Id], f_body :: Stm, f_retval :: Expr }
   deriving (Show)
                  
type Program = [Function]
