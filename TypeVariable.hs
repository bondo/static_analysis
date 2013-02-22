module TypeVariable where

import Ast (Expr(..), iVal, showPar)
import DisjointSet (Elem(..))

import Data.List (intercalate)

type TVID = Int
data TypeVariable = TVInt { tvId :: TVID }
                  | TVRef { tvInner :: TypeVariable, tvId :: TVID }
                  | TVFun { tvFormals :: [TypeVariable], tvRetval :: TypeVariable, tvId :: TVID }
                  | TVExp { tvExpr :: Expr, tvId :: TVID } -- [[E]]
                  | TVGen { tvId :: TVID } -- alpha

instance Eq TypeVariable where
  (TVExp e1 _)    == (TVExp e2 _)    = e1 == e2
  (TVRef e1 _)    == (TVRef e2 _)    = e1 == e2
  (TVFun f1 r1 _) == (TVFun f2 r2 _) = f1 == f2 && r1 == r2
  a               == b               = tvId a == tvId b

compat :: TypeVariable -> TypeVariable -> Bool
TVInt _       `compat` TVInt _       = True
TVRef v1 _    `compat` TVRef v2 _    = True
TVGen _       `compat` _             = True
_             `compat` TVGen _       = True
TVFun f1 r1 _ `compat` TVFun f2 r2 _ = length f1 == length f2 && all id (zipWith compat f1 f2) && compat r1 r2
TVExp _ _     `compat` _             = True
_             `compat` TVExp _ _     = True
_             `compat` _             = False

squares :: String -> String
squares s = "[" ++ s ++ "]"

showExpr :: Expr -> String
showExpr (EConst v u)           = show v
showExpr (EVar n)               = iVal n
showExpr (EBinOp op l r u)      = showExpr l ++ " " ++ show op ++ " " ++ showExpr r
showExpr (EAppNamed n args u)   = iVal n ++ showPar (intercalate ", " $ map showExpr args)
showExpr (EAppUnnamed e args u) = showPar (showExpr e) ++ showPar (intercalate ", " $ map showExpr args)
showExpr (ERef n u)             = '&' : iVal n
showExpr (EDeRef e u)           = '*' : showExprPar e
showExpr (EInput u)             = "input"
showExpr (EMalloc u)            = "malloc"
showExpr (ENull u)              = "null"

showExprPar :: Expr -> String
showExprPar (EConst v u) = show v
showExprPar (EVar n)     = iVal n
showExprPar (EInput u)   = "input"
showExprPar (EMalloc u)  = "malloc"
showExprPar (ENull u)    = "null"
showExprPar e            = showPar $ showExpr e

instance Show TypeVariable where
  show (TVInt i)     = "int"
  show (TVRef t i)   = '&' : show t
  show (TVExp e i)   = squares $ showExpr e
  show (TVFun f r i) = "(" ++ intercalate ", " (map show f) ++ ")->" ++ show r
  show (TVGen i)     = "a"

instance Elem TypeVariable where
  bestRep (TVGen _)   b           = b
  bestRep (TVExp _ _) b           = b
  bestRep a           _           = a
