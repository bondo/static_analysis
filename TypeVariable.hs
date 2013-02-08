module TypeVariable where

import Ast
import DisjointSet

import Data.List (intercalate)

type TVID = Int
data TypeVariable = TVInt    { tv_id :: TVID }
                  | TVRef    { tv_expr :: Expr, tv_id :: TVID }
                  | TVGenRef { tv_id :: TVID } -- &a
                  | TVFun    { tv_formals :: [TypeVariable], tv_retval :: TypeVariable, tv_id :: TVID }
                  | TVVar    { tv_expr :: Expr, tv_id :: TVID }

instance Eq TypeVariable where
  (TVVar e1 _) == (TVVar e2 _) = e_uid e1 == e_uid e2
  (TVRef e1 _) == (TVRef e2 _) = e_uid e1 == e_uid e2
  a            == b            = tv_id a  == tv_id b

compat :: TypeVariable -> TypeVariable -> Bool
TVInt _       `compat` TVInt _       = True
TVRef _ _     `compat` TVRef _ _     = True
TVGenRef _    `compat` TVRef _ _     = True
TVRef _ _     `compat` TVGenRef _    = True
TVGenRef _    `compat` TVGenRef _    = True
TVFun f1 r1 _ `compat` TVFun f2 r2 _ = length f1 == length f2 && all id (zipWith compat f1 f2) && compat r1 r2
TVVar _ _     `compat` _             = True
_             `compat` TVVar _ _     = True
_             `compat` _             = False

squares :: String -> String
squares s = "[" ++ s ++ "]"

showExpr :: Expr -> String
showExpr (EConst v u)           = show v
showExpr (EVar n)               = i_val n
showExpr (EBinOp op l r u)      = showExpr l ++ " " ++ show op ++ " " ++ showExpr r
showExpr (EAppNamed n args u)   = i_val n ++ showPar (intercalate ", " $ map showExpr args)
showExpr (EAppUnnamed e args u) = showPar (showExpr e) ++ showPar (intercalate ", " $ map showExpr args)
showExpr (ERef n u)             = "&" ++ i_val n
showExpr (EDeRef e u)           = "*" ++ showExprPar e
showExpr (EInput u)             = "input"
showExpr (EMalloc u)            = "malloc"
showExpr (ENull u)              = "null"

showExprPar :: Expr -> String
showExprPar (EConst v u) = show v
showExprPar (EVar n)     = i_val n
showExprPar (EInput u)   = "input"
showExprPar (EMalloc u)  = "malloc"
showExprPar (ENull u)    = "null"
showExprPar e            = showPar $ showExpr e

instance Show TypeVariable where
  show (TVInt i)     = "int"
  show (TVRef e i)   = "&" ++ squares (showExpr e)
  show (TVVar e i)   = squares $ showExpr e
  show (TVFun f r i) = "(" ++ (intercalate ", " $ map show f) ++ ")->" ++ show r
  show (TVGenRef i)  = "&a"

instance Elem TypeVariable where
  bestRep (TVVar _ _)  b            = b
  bestRep a            (TVVar _ _)  = a
  bestRep (TVGenRef _) b            = b
  bestRep a            (TVGenRef _) = a
  bestRep a            b            = a
