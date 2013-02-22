module Ast where

import Control.DeepSeq (NFData(rnf))
import Data.List (intercalate)
import Text.PrettyPrint

type GUID = Int

data Id = Id { iVal :: String, iUid :: GUID }

data Expr = -- E
    EConst      { eVal  :: Int, eUid' :: GUID }
  | EVar        { eName :: Id }
  | EBinOp      { eOp   :: BinOp, eLeft :: Expr, eRight :: Expr, eUid' :: GUID }
  | EAppNamed   { eName :: Id,   eArgs :: [Expr], eUid' :: GUID } -- id(E,...,E)
  | EAppUnnamed { eExpr :: Expr, eArgs :: [Expr], eUid' :: GUID } -- (E)(E,...,E)
  | ERef        { eName :: Id,   eUid' :: GUID } -- &id
  | EDeRef      { eExpr :: Expr, eUid' :: GUID } -- *E
  | EInput      { eUid' :: GUID }
  | EMalloc     { eUid' :: GUID }
  | ENull       { eUid' :: GUID }
eUid (EVar n) = iUid n
eUid e        = eUid' e

data BinOp = BPlus
           | BMinus
           | BMulti
           | BDiv
           | BGt
           | BEq

data Stm = SAss    { sName :: Id, sVal :: Expr }
         | SAssRef { sName :: Id, sVal :: Expr } -- *id = E
         | SOutput { sVal  :: Expr }
         | SSeq    { sStms :: [Stm] }
         | SIfElse { sCond :: Expr, sThen :: Stm, sElse :: Stm }
         | SWhile  { sCond :: Expr, sBody :: Stm }
         | SDecl   { sIds  :: [Id] }
         | SReturn { sVal  :: Expr } -- Only use this to simplify parsing, weed it out later
         | SNop -- Has no syntax

data Function =
  -- Use when there's SReturns in Stm, convert to FNamed in the weeder
     FNamedSimple { fName :: Id, fFormals :: [Id], fBody :: Stm }
   | FNamed       { fName :: Id, fFormals :: [Id], fBody :: Stm, fRetval :: Expr }

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
  show i = iVal i ++ showUid (iUid i)

instance Eq Id where
  i1 == i2 = iUid i1 == iUid i2
  
instance Ord Id where
  i1 `compare` i2 = iUid i1 `compare` iUid i2

instance Eq Expr where
  (EDeRef e1 _) == (EDeRef e2 _) = e1 == e2
  a             == b             = eUid a == eUid b

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

instance NFData Id where
  rnf (Id val uid) = rnf (val, uid)

instance NFData BinOp

instance NFData Expr where
  rnf (EConst v u)            = rnf (v, u)
  rnf (EVar n)                = rnf n
  rnf (EBinOp op l r u)       = rnf (op, l, r, u)
  rnf (EAppNamed n args u)    = rnf (n, args, u)
  rnf (EAppUnnamed e args u)  = rnf (e, args, u)
  rnf (ERef n u)              = rnf n
  rnf (EDeRef e u)            = rnf (e, u)
  rnf (EInput u)              = rnf u
  rnf (EMalloc u)             = rnf u
  rnf (ENull u)               = rnf u
