module Constraints (Constraint, showConstraint, showConstraints, generateConstraints, intIdConst) where

import Ast (Expr(..), Stm(..), Program, Function(..), BinOp(..), i_val)
import TypeVariable (TypeVariable(..), TVID)

import Control.Monad (liftM)
import Control.Monad.State
import Data.List (intercalate)

type Constraint = (TypeVariable, TypeVariable)

showConstraint :: Constraint -> String
showConstraint (a, b) = show a ++ "  =  " ++ show b

showConstraints :: [Constraint] -> String
showConstraints cs = intercalate "\n" $ map showConstraint cs

generateConstraints :: Program -> [Constraint]
generateConstraints prog = extract $ concatMapM genFunction prog

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f l = mapM f l >>= return . concat

genFunction :: Function -> IdGen [Constraint]
genFunction (FNamedSimple name _ _) =
  error $ "Cannot generate constraints for the function " ++ i_val name ++ ": Not in normal form (not weeded)."
genFunction (FNamed name formals body retval) = do
  c_left    <- genVar $ EVar name  
  c_formals <- mapM (genVar . EVar) formals
  c_retval  <- genVar retval
  c_right   <- genFun c_formals c_retval  
  c_body    <- genStm body
  c_retRec  <- genExpr retval
  return ((c_left, c_right) : c_body ++ c_retRec)

genStm :: Stm -> IdGen [Constraint]
genStm (SAss name val) = do
  c_left  <- genVar $ EVar name
  c_right <- genVar val
  c_val   <- genExpr val
  return $ (c_left, c_right) : c_val
genStm (SAssRef name val) = do
  c_left  <- genVar $ EVar name
  c_right <- genRef val
  c_val   <- genExpr val
  return $ (c_left, c_right) : c_val
genStm (SOutput val) = do
  c_left  <- genVar val
  c_right <- genInt
  c_val   <- genExpr val
  return $ (c_left, c_right) : c_val
genStm (SSeq ss) = concatMapM genStm ss
genStm (SIfElse cond s1 s2) = do
  c_left  <- genVar cond
  c_right <- genInt
  c_s1    <- genStm s1
  c_s2    <- genStm s2
  c_cond  <- genExpr cond
  return $ (c_left, c_right) : c_cond ++ c_s1 ++ c_s2
genStm (SWhile cond body) = do
  c_left  <- genVar cond
  c_right <- genInt
  c_body  <- genStm body
  c_cond  <- genExpr cond
  return $ (c_left, c_right) : c_cond ++ c_body
genStm (SDecl _) = return []
genStm (SReturn val) = error $ "Return statemente are only allowed as the last thing in a function."
genStm SNop = return []

genExpr :: Expr -> IdGen [Constraint]
genExpr e@(EConst _ _) = do
  c_e <- genVar e
  c_i <- genInt
  return [(c_e, c_i)]
genExpr (EVar _) = return []
genExpr e@(EBinOp BEq left right _) = do
  c_left     <- genVar left
  c_right    <- genVar right
  c_op       <- genVar e
  c_int      <- genInt
  c_leftRec  <- genExpr left
  c_rightRec <- genExpr right
  return $ [(c_left, c_right), (c_op, c_int)] ++ c_leftRec ++ c_rightRec
genExpr e@(EBinOp op left right _) = do
  c_left     <- genVar left
  c_right    <- genVar right
  c_op       <- genVar e
  c_int      <- genInt
  c_leftRec  <- genExpr left
  c_rightRec <- genExpr right
  return $ [(c_left, c_int), (c_right, c_int), (c_op, c_int)] ++ c_leftRec ++ c_rightRec
genExpr e@(EAppNamed name args _) = do
  c_left    <- genVar $ EVar name
  c_formals <- mapM genVar args
  c_retval  <- genVar e
  c_right   <- genFun c_formals c_retval
  c_args    <- concat `liftM` mapM genExpr args
  return $ (c_left, c_right) : c_args
genExpr e@(EAppUnnamed expr args _) = do
  c_left    <- genVar expr
  c_formals <- mapM genVar args
  c_retval  <- genVar e
  c_right   <- genFun c_formals c_retval
  c_args    <- concat `liftM` mapM genExpr args
  c_expr    <- genExpr expr
  return $ (c_left, c_right) : c_expr ++ c_args
genExpr e@(ERef name _) = do
  c_e    <- genVar e
  c_name <- genRef $ EVar name
  return [(c_e, c_name)]
genExpr e@(EDeRef expr _) = do
  c_expr <- genVar expr
  c_e    <- genRef e
  c_rec  <- genExpr expr
  return $ (c_expr, c_e) : c_rec
genExpr e@(EInput _) = do
  c_e   <- genVar e
  c_int <- genInt
  return [(c_e, c_int)]
genExpr e@(EMalloc _) = do
  c_e      <- genVar e
  c_genRef <- genGenRef
  return [(c_e, c_genRef)]
genExpr e@(ENull _) = do
  c_e      <- genVar e
  c_genRef <- genGenRef
  return [(c_e, c_genRef)]


type IdGen = State TVID

gen :: (TVID -> TypeVariable) -> IdGen TypeVariable
gen c = do id <- get
           put $ id + 1
           return $ c id

genInt :: IdGen TypeVariable
genInt = return $ TVInt intIdConst

genGenRef :: IdGen TypeVariable
genGenRef = gen TVGenRef

genVar :: Expr -> IdGen TypeVariable
genVar = gen . TVVar

genRef :: Expr -> IdGen TypeVariable
genRef e = genVar e >>= gen . TVRef

genFun :: [TypeVariable] -> TypeVariable -> IdGen TypeVariable
genFun formals retval = gen $ TVFun formals retval

intIdConst :: Int
intIdConst = 1

extract :: IdGen a -> a
extract ig = evalState ig 2