module Constraints (Constraint, showConstraint, showConstraints, generateConstraints) where

import Ast (Expr(..), Stm(..), Program, Function(..), BinOp(..), Id, iVal)
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
  error $ "Cannot generate constraints for the function " ++ iVal name ++ ": Not in normal form (not weeded)."
genFunction (FNamed name formals body retval) = do
  cLeft    <- genExp $ EVar name  
  cFormals <- mapM (genExp . EVar) formals
  cRetval  <- genExp retval
  cRight   <- genFun cFormals cRetval  
  c_body    <- genStm body
  cRetRec  <- genExpr retval
  return ((cLeft, cRight) : c_body ++ cRetRec)

genStm :: Stm -> IdGen [Constraint]
genStm (SAss name val)    = genAssCont name val genExp
genStm (SAssRef name val) = genAssCont name val genRef
genStm (SSeq ss)          = concatMapM genStm ss
genStm (SDecl _)          = return []
genStm (SReturn val)      = error "Return statemente are only allowed as the last thing in a function."
genStm SNop               = return []
genStm (SOutput val) = do
  cLeft  <- genExp val
  cRight <- genInt
  cVal   <- genExpr val
  return $ (cLeft, cRight) : cVal
genStm (SIfElse cond s1 s2) = do
  cLeft  <- genExp cond
  cRight <- genInt
  c_s1    <- genStm s1
  c_s2    <- genStm s2
  c_cond  <- genExpr cond
  return $ (cLeft, cRight) : c_cond ++ c_s1 ++ c_s2
genStm (SWhile cond body) = do
  cLeft  <- genExp cond
  cRight <- genInt
  c_body  <- genStm body
  c_cond  <- genExpr cond
  return $ (cLeft, cRight) : c_cond ++ c_body

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
  cE    <- genExp e
  c_name <- genRef $ EVar name
  return [(cE, c_name)]
genExpr e@(EDeRef expr _) = do
  cExpr <- genExp expr
  cE    <- genRef e
  cRec  <- genExpr expr
  return $ (cExpr, cE) : cRec


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
  let left  = eLeft e
      right = eRight e
  cLeft     <- genExp left
  cRight    <- genExp right
  cOp       <- genExp e
  cInt      <- genInt
  cLeftRec  <- genExpr left
  cRightRec <- genExpr right
  if isEq
    then return $ [(cLeft, cRight), (cOp, cInt)] ++ cLeftRec ++ cRightRec
    else return $ [(cLeft, cInt), (cRight, cInt), (cOp, cInt)] ++ cLeftRec ++ cRightRec

genFunCont :: Expr -> Expr-> [Expr] -> IdGen [Constraint]
genFunCont e name args = do
  cLeft    <- genExp name
  cFormals <- mapM genExp args
  cRetval  <- genExp e
  cRight   <- genFun cFormals cRetval
  cArgs    <- concat `liftM` mapM genExpr args
  cExpr    <- genExpr name
  return $ (cLeft, cRight) : cExpr ++ cArgs

genGenRefCont :: Expr -> IdGen [Constraint]
genGenRefCont e = do
  cE      <- genExp e
  cGenRef <- genGenRef
  return [(cE, cGenRef)]

genExpIntCont :: Expr -> IdGen [Constraint]
genExpIntCont e = do
  cE <- genExp e
  cI <- genInt
  return [(cE, cI)]

genAssCont :: Id -> Expr -> (Expr -> IdGen TypeVariable) -> IdGen [Constraint]
genAssCont name val wrap = do
  cLeft  <- genExp $ EVar name
  cRight <- wrap val
  cVal   <- genExpr val
  return $ (cLeft, cRight) : cVal

intIdConst :: Int
intIdConst = 1

extract :: IdGen a -> a
extract ig = evalState ig 2
