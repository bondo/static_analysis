module UnifyTypes (DS_TV, unify, unifyAll, getTypes) where

import Ast (Expr)
import Constraints (Constraint)
import DisjointSet (union, find, result, toList, DisjointSet)
import TypeVariable (TypeVariable(..), compat)
import Types (Type(..))

import Control.Arrow ((&&&))
import Control.Monad (zipWithM_, unless, liftM, when, forM, ap)
import qualified Data.Set as Set
import qualified Data.IntSet as ISet
import Data.List (intercalate)
import Data.Maybe (isJust, fromJust)

type DS_TV = DisjointSet TypeVariable
type Set = Set.Set (Int, Int)

unifyAll :: [Constraint] -> DS_TV ()
unifyAll = mapM_ unify

unify :: Constraint -> DS_TV ()
unify (ta, tb) = findAndUnify Set.empty ta tb

findAndUnify :: Set -> TypeVariable -> TypeVariable -> DS_TV ()
findAndUnify s ta tb = do
  ta' <- find ta
  tb' <- find tb
  unless (Set.member ids s) $ unify' (Set.insert ids s) ta' tb'
  where ids = (tv_id ta, tv_id tb)

unify' :: Set -> TypeVariable -> TypeVariable -> DS_TV ()
unify' s ta@(TVInt _)       tb@(TVInt _)       = ta `union` tb
unify' s ta@(TVRef t1 _)    tb@(TVRef t2 _)    = ta `union` tb >> unifyIfEqual s t1 t2
unify' s ta@(TVFun fa ra _) tb@(TVFun fb rb _) =
  union ta tb >> unifyIfEqual s ra rb >> zipWithM_ (unifyIfEqual s) fa fb
unify' s ta@(TVGen _)       tb                 = ta `union` tb
unify' s ta                 tb@(TVGen _)       = ta `union` tb
unify' s ta@(TVExp _ _)     tb                 = ta `union` tb
unify' s ta                 tb@(TVExp _ _)     = ta `union` tb
unify' s ta                 tb                 = failedUnification ta tb

unifyIfEqual :: Set -> TypeVariable -> TypeVariable -> DS_TV ()
unifyIfEqual s ta tb | ta `compat` tb = findAndUnify s ta tb
                     | otherwise      = failedUnification ta tb

failedUnification :: TypeVariable -> TypeVariable -> a
failedUnification ta tb = error $ "Unable to unify [" ++ show ta ++ "] with [" ++ show tb ++ "]."


getTypes :: [Constraint] -> [(Expr, Type)]
getTypes = getTypesFromDS . unifyAll

getTypesFromDS :: DS_TV a -> [(Expr, Type)]
getTypesFromDS ds = concatMap (map (tv_expr &&& getType ds) . filter isExpr) (toList ds)
  where isExpr e | TVExp{} <- e = True
                 | otherwise    = False

getType :: DS_TV a -> TypeVariable -> Type
getType ds = getType' ds []

getType' :: DS_TV a -> [(Int, Type)] -> TypeVariable -> Type
getType' ds ids t =
  let t'   = result $ ds >> find t
      tid  = tv_id t'
      old  = lookup tid ids
      ty   = getTypeNoCheck ds nids t'
      nids = (tid, ty) : ids in
  maybe ty TReg old

missingInfo = "Not enough information to infer type of "

getTypeNoCheck :: DS_TV a -> [(Int, Type)] -> TypeVariable -> Type
getTypeNoCheck ds ids (TVInt _)       = TInt
getTypeNoCheck ds ids (TVRef inner _) = TRef $ getType' ds ids inner
getTypeNoCheck ds ids (TVExp expr _)  = error $ missingInfo ++ show expr
getTypeNoCheck ds ids (TVGen _)       = error $ missingInfo ++ "'alpha'"
getTypeNoCheck ds ids (TVFun formals retval _) =
  let tformals = map (getType' ds ids) formals
      tretval  = getType' ds ids retval in
  TFun tformals tretval
