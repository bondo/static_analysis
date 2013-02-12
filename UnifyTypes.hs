module UnifyTypes (DS_TV, unify, unifyAll) where

import Constraints (Constraint)
import DisjointSet (union, find, DisjointSet)
import TypeVariable (TypeVariable(..), compat)

import Control.Monad (zipWithM_, unless)
import qualified Data.Set as Set
import Data.List (intercalate)

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
unify' s ta@(TVGenRef _)    tb@(TVRef _ _)     = ta `union` tb
unify' s ta@(TVRef _ _)     tb@(TVGenRef _)    = ta `union` tb
unify' s ta@(TVGenRef _)    tb@(TVGenRef _)    = ta `union` tb
unify' s ta@(TVFun fa ra _) tb@(TVFun fb rb _) =
  union ta tb >> unifyIfEqual s ra rb >> zipWithM_ (unifyIfEqual s) fa fb
unify' s ta@(TVVar _ _)     tb                 = ta `union` tb
unify' s ta                 tb@(TVVar _ _)     = ta `union` tb
unify' s ta                 tb                 = failedUnification ta tb

unifyIfEqual :: Set -> TypeVariable -> TypeVariable -> DS_TV ()
unifyIfEqual s ta tb | ta `compat` tb = findAndUnify s ta tb
                     | otherwise      = failedUnification ta tb

failedUnification :: TypeVariable -> TypeVariable -> a
failedUnification ta tb = error $ "Unable to unify [" ++ show ta ++ "] with [" ++ show tb ++ "]."
