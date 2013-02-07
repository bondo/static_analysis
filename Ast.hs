module Ast where

import Data.Foldable
import Data.List (intercalate)
import Data.Monoid hiding ((<>))
import Text.PrettyPrint

type GUID = Int

data Id = Id { i_val :: String, i_uid :: GUID }

data Expr = -- E
    EConst      { e_val  :: Int, e_uid :: GUID }
  | EVar        { e_name :: Id }
  | EBinOp      { e_op   :: BinOp, e_left :: Expr, e_right :: Expr, e_uid :: GUID }
  | EAppNamed   { e_name :: Id,   e_args :: [Expr], e_uid :: GUID } -- id(E,...,E)
  | EAppUnnamed { e_expr :: Expr, e_args :: [Expr], e_uid :: GUID } -- (E)(E,...,E)
  | ERef        { e_name :: Id,   e_uid :: GUID } -- &id
  | EDeRef      { e_expr :: Expr, e_uid :: GUID } -- *E
  | EInput      { e_uid  :: GUID }
  | EMalloc     { e_uid  :: GUID }
  | ENull       { e_uid  :: GUID }

data BinOp = BPlus
           | BMinus
           | BMulti
           | BDiv
           | BGt
           | BEq

data Stm = SAss    { s_name :: Id, s_val :: Expr }
         | SAssRef { s_name :: Id, s_val :: Expr } -- *id = E
         | SOutput { s_val  :: Expr }
         | SSeq    { s_stms :: [Stm] }
         | SIfElse { s_cond :: Expr, s_then :: Stm, s_else :: Stm }
         | SWhile  { s_cond :: Expr, s_body :: Stm }
         | SDecl   { s_ids  :: [Id] }
         | SReturn { s_val  :: Expr } -- Only use this to simplify parsing, weed it out later
         | SNop -- Has no syntax

data Function =
  -- Use when there's SReturns in Stm, convert to FNamed in the weeder
     FNamedSimple { f_name :: Id, f_formals :: [Id], f_body :: Stm }
   | FNamed       { f_name :: Id, f_formals :: [Id], f_body :: Stm, f_retval :: Expr }

type Program = [Function]

-- Helper functions

showUid :: GUID -> String
showUid u = "{" ++ show u ++ "}"

showPar :: String -> String
showPar s = "(" ++ s ++ ")"

showProgram :: Program -> String
showProgram p = intercalate "\n\n" $ map show p

commasep :: [String] -> Doc
commasep xs = hcat . punctuate (comma <> space) $ map text xs

tabSize = 4

doc :: Stm -> Doc
doc (SAss n v)      = text (show n) <+> char '=' <+> text (show v) <> char ';'
doc (SAssRef n v)   = char '&' <> text (show n) <+> char '=' <+> text (show v) <> char ';'
doc (SOutput v)     = text "output" <+> text (show v) <> char ';'
doc (SSeq ss)       = vcat $ map doc ss
doc (SIfElse c t e) = vcat [ text "if" <+> text (show c) <+> lbrace
                           , nest tabSize $ doc t
                           , rbrace <+> text "else" <+> lbrace
                           , nest tabSize $ doc e
                           , rbrace
                           ]
doc (SWhile c b)    = vcat [ text "while" <+> text (show c) <+> lbrace
                           , nest tabSize $ doc b
                           , rbrace
                           ]
doc (SDecl ids)     = text "var" <+> commasep (map show ids) <> char ';'
doc (SReturn v)     = text "return" <+> text (show v) <> char ';'
doc (SNop)          = text "nop" <> char ';'


-- Instances

instance Show Id where
  show i = i_val i ++ (showUid $ i_uid i)

instance Eq Expr where
  a == b = e_uid a == e_uid b

instance Show Expr where
  show (EConst v u)           = show v                                                         ++ showUid u
  show (EVar n)               = show n
  show (EBinOp op l r u)      = showPar (show l ++ " " ++ show op ++ " " ++ show r)            ++ showUid u
  show (EAppNamed n args u)   = show n ++ showPar (intercalate ", " $ map show args)           ++ showUid u
  show (EAppUnnamed e args u) = showPar (show e) ++ showPar (intercalate ", " $ map show args) ++ showUid u
  show (ERef n u)             = "(&" ++ show n ++ ")"                                          ++ showUid u
  show (EDeRef e u)           = "*" ++ showPar (show e)                                        ++ showUid u
  show (EInput u)             = "input"                                                        ++ showUid u
  show (EMalloc u)            = "malloc"                                                       ++ showUid u
  show (ENull u)              = "null"                                                         ++ showUid u

instance Show BinOp where
  show BPlus  = "+"
  show BMinus = "-"
  show BMulti = "*"
  show BDiv   = "/"
  show BGt    = ">"
  show BEq    = "=="

instance Show Stm where
  show = render . doc

instance Show Function where
  show (FNamedSimple n f b) = render $ vcat [ text (show n) <> parens (commasep (map show f)) <+> lbrace
                                            , nest tabSize $ doc b
                                            , rbrace
                                            ]
  show (FNamed n f b r) = render $ vcat [ text (show n) <> parens (commasep (map show f)) <+> lbrace
                                        , nest tabSize $ doc b
                                        , nest tabSize (text "return" <+> text (show r) <> char ';')
                                        , rbrace
                                        ]
