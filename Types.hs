module Types where

import Ast (Expr, e_uid)
import DisjointSet

import Data.List (intercalate)


data Type = TInt
          | TRef Type
          | TFun [Type] Type
          | TVar -- Type variable

data HalfTypedExpr = HTEInt { hte_expr :: Expr }
                   | HTERef { hte_expr :: Expr }
                   | HTEFun { hte_expr :: Expr }
                   | HTEVar { hte_expr :: Expr }


parens a = "(" ++ a ++ ")"

showFun dom cod = intercalate " -> " (map showFunParens dom) ++ " -> " ++ show cod
  where showFunParens f@(TFun _ _) = parens $ show f
        showFunParens t            = show t


instance Eq Type where
  TInt        == TInt         = True
  TRef t1     == TRef t2      = t1 == t2
  TFun d1 c1  == TFun d2 c2   = c1 == c2 && (all id $ zipWith (==) d1 d2)
  TVar        == _            = True
  _           == TVar         = True
  _           == _            = False

instance Show Type where
  show TInt           = "Int"
  show (TRef t)       = "&" ++ show t
  show (TFun dom cod) = showFun dom cod
  show TVar           = "?"

instance Eq HalfTypedExpr where
  HTEInt _ == HTEInt _ = True
  HTERef _ == HTERef _ = True
  HTEFun _ == HTEFun _ = True
  HTEVar _ == _        = True
  _        == HTEVar _ = True
  _        == _        = False

instance Show HalfTypedExpr where
  show (HTEInt e) = parens (show e) ++ " :: Int"
  show (HTERef e) = parens (show e) ++ " :: &?"
  show (HTEFun e) = parens (show e) ++ " :: ? -> ?"
  show (HTEVar e) = parens (show e) ++ " :: ?"

instance Elem HalfTypedExpr where
  eq a b = uid a == uid b
    where uid = e_uid . hte_expr
  bestRep (HTEVar _) b = b
  bestRep a _          = a
