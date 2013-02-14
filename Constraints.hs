module Constraints (Constraint, showConstraint, showConstraints, generateConstraints) where

import Ast (Expr(..), Stm(..), Program, Function(..), BinOp(..), Id, i_val)
import TypeVariable (TypeVariable(..), TVID)

import Control.Monad (liftM)
import Control.Monad.State
import Data.List (intercalate)

type Constraint = (TypeVariable, TypeVariable)
type IdGen = State TVID


showConstraint :: Constraint -> String
showConstraint (a, b) = show a ++ "  =  " ++ show b

showConstraints :: [Constraint] -> String
showConstraints cs = intercalate "\n" $ map showConstraint cs

generateConstraints :: Program -> [Constraint]
generateConstraints prog = extract $ concatMapM genFunction prog

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f l = concat `liftM` mapM f l

genFunction :: Function -> IdGen [Constraint]
genFunction (FNamedSimple name _ _) =
  error $ "Cannot generate constraints for the function " ++ i_val name ++ ": Not in normal form (not weeded)."
genFunction (FNamed name formals body retval) = do
  c_left    <- genExp $ EVar name  
  c_formals <- mapM (genExp . EVar) formals
  c_retval  <- genExp retval
  c_right   <- genFun c_formals c_retval  
  c_body    <- genStm body
  c_retRec  <- genExpr retval
  return ((c_left, c_right) : c_body ++ c_retRec)

genStm :: Stm -> IdGen [Constraint]
genStm (SAss name val)    = genAssCont name val genExp
genStm (SAssRef name val) = genAssCont name val genRef
genStm (SSeq ss)          = concatMapM genStm ss
genStm (SDecl _)          = return []
genStm (SReturn val)      = error "Return statemente are only allowed as the last thing in a function."
genStm SNop               = return []
genStm (SOutput val) = do
  c_left  <- genExp val
  c_right <- genInt
  c_val   <- genExpr val
  return $ (c_left, c_right) : c_val
genStm (SIfElse cond s1 s2) = do
  c_left  <- genExp cond
  c_right <- genInt
  c_s1    <- genStm s1
  c_s2    <- genStm s2
  c_cond  <- genExpr cond
  return $ (c_left, c_right) : c_cond ++ c_s1 ++ c_s2
genStm (SWhile cond body) = do
  c_left  <- genExp cond
  c_right <- genInt
  c_body  <- genStm body
  c_cond  <- genExpr cond
  return $ (c_left, c_right) : c_cond ++ c_body

genExpr :: Expr -> IdGen [Constraint]
genExpr e@(EConst _ _)              = genExpIntCont e
genExpr (EVar _)                    = return []
genExpr e@(EBinOp BEq _ _ _)        = genOpCont e True
genExpr e@(EBinOp{})                = genOpCont e False
genExpr e@(EAppNamed name args _)   = genFunCont e (EVar name) args
genExpr e@(EAppUnnamed expr args _) = genFunCont e expr args
genExpr e@(EInput _)                = genExpIntCont e
genExpr e@(EMalloc _)               = genGenRefCont e
genExpr e@(ENull _)                 = genGenRefCont e
genExpr e@(ERef name _) = do
  c_e    <- genExp e
  c_name <- genRef $ EVar name
  return [(c_e, c_name)]
genExpr e@(EDeRef expr _) = do
  c_expr <- genExp expr
  c_e    <- genRef e
  c_rec  <- genExpr expr
  return $ (c_expr, c_e) : c_rec


gen :: (TVID -> TypeVariable) -> IdGen TypeVariable
gen c = do id <- get
           put $ id + 1
           return $ c id

genInt :: IdGen TypeVariable
genInt = return $ TVInt intIdConst

genGen :: IdGen TypeVariable
genGen = gen TVGen

genGenRef :: IdGen TypeVariable
genGenRef = genGen >>= gen . TVRef

genExp :: Expr -> IdGen TypeVariable
genExp = gen . TVExp

genRef :: Expr -> IdGen TypeVariable
genRef e = genExp e >>= gen . TVRef

genFun :: [TypeVariable] -> TypeVariable -> IdGen TypeVariable
genFun formals retval = gen $ TVFun formals retval

genOpCont :: Expr -> Bool -> IdGen [Constraint]
genOpCont e isEq = do
  let left  = e_left e
      right = e_right e
  c_left     <- genExp left
  c_right    <- genExp right
  c_op       <- genExp e
  c_int      <- genInt
  c_leftRec  <- genExpr left
  c_rightRec <- genExpr right
  if isEq
    then return $ [(c_left, c_right), (c_op, c_int)] ++ c_leftRec ++ c_rightRec
    else return $ [(c_left, c_int), (c_right, c_int), (c_op, c_int)] ++ c_leftRec ++ c_rightRec

genFunCont :: Expr -> Expr-> [Expr] -> IdGen [Constraint]
genFunCont e name args = do
  c_left    <- genExp name
  c_formals <- mapM genExp args
  c_retval  <- genExp e
  c_right   <- genFun c_formals c_retval
  c_args    <- concat `liftM` mapM genExpr args
  c_expr    <- genExpr name
  return $ (c_left, c_right) : c_expr ++ c_args

genGenRefCont :: Expr -> IdGen [Constraint]
genGenRefCont e = do
  c_e      <- genExp e
  c_genRef <- genGenRef
  return [(c_e, c_genRef)]

genExpIntCont :: Expr -> IdGen [Constraint]
genExpIntCont e = do
  c_e <- genExp e
  c_i <- genInt
  return [(c_e, c_i)]

genAssCont :: Id -> Expr -> (Expr -> IdGen TypeVariable) -> IdGen [Constraint]
genAssCont name val wrap = do
  c_left  <- genExp $ EVar name
  c_right <- wrap val
  c_val   <- genExpr val
  return $ (c_left, c_right) : c_val

intIdConst :: Int
intIdConst = 1

extract :: IdGen a -> a
extract ig = evalState ig 2