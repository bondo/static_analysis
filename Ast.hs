module Ast where

import Data.Foldable
import Data.List (intercalate)
import Data.Monoid hiding ((<>))
import Text.PrettyPrint

type GUID = Int
type Id = String
type Expr = Expression GUID

data Expression a = -- E
    EConst      { e_val  :: Int, e_uid :: a } -- intconst
  | EVar        { e_name :: Id,  e_uid :: a } -- id
  | EBinOp      { e_op   :: BinOp, e_left :: Expression a, e_right :: Expression a, e_uid :: a } -- E BinOp E
  | EAppNamed   { e_name :: Id,           e_args :: [Expression a], e_uid :: a } -- id(E,...,E)
  | EAppUnnamed { e_expr :: Expression a, e_args :: [Expression a], e_uid :: a } -- (E)(E,...,E)
  | ERef        { e_name :: Id,           e_uid :: a } -- &id
  | EDeRef      { e_expr :: Expression a, e_uid :: a } -- *E
  | EInput      { e_uid  :: a }
  | EMalloc     { e_uid  :: a }
  | ENull       { e_uid  :: a }

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

showUid :: Show a => a -> String
showUid u = "{" ++ show u ++ "}"

showPar :: String -> String
showPar s = "(" ++ s ++ ")"

showProgram :: Program -> String
showProgram p = intercalate "\n\n" $ map show p

commasep :: [String] -> Doc
commasep xs = hcat . punctuate (comma <> space) $ map text xs

tabSize = 4

doc :: Stm -> Doc
doc (SAss n v)      = text n <+> char '=' <+> text (show v) <> char ';'
doc (SAssRef n v)   = char '&' <> text n <+> char '=' <+> text (show v) <> char ';'
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
doc (SDecl ids)     = text "var" <+> commasep ids <> char ';'
doc (SReturn v)     = text "return" <+> text (show v) <> char ';'
doc (SNop)          = text "nop" <> char ';'


-- Instances

instance Eq a => Eq (Expression a) where
  a == b = e_uid a == e_uid b

instance Foldable Expression where
  foldMap f (EBinOp _ l r u) = foldMap f l `mappend` f u `mappend` foldMap f r
  foldMap f (EAppNamed _ args u) = mconcat (map (foldMap f) args) `mappend` f u
  foldMap f e = f $ e_uid e

instance Show a => Show (Expression a) where
  show (EConst v u)           = show v                                                         ++ showUid u
  show (EVar n u)             = n                                                              ++ showUid u
  show (EBinOp op l r u)      = showPar (show l ++ " " ++ show op ++ " " ++ show r)            ++ showUid u
  show (EAppNamed n args u)   = n ++ showPar (intercalate ", " $ map show args)                ++ showUid u
  show (EAppUnnamed e args u) = showPar (show e) ++ showPar (intercalate ", " $ map show args) ++ showUid u
  show (ERef n u)             = "&" ++ n                                                       ++ showUid u
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
  show (FNamedSimple n f b) = render $ vcat [ text n <> parens (commasep f) <+> lbrace
                                            , nest tabSize $ doc b
                                            , rbrace
                                            ]
  show (FNamed n f b r) = render $ vcat [ text n <> parens (commasep f) <+> lbrace
                                        , nest tabSize $ doc b
                                        , nest tabSize (text "return" <+> text (show r) <> char ';')
                                        , rbrace
                                        ]
