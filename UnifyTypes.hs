module UnifyTypes (DS_TV, unify, unifyAll) where

import Constraints
import DisjointSet
import TypeVariable

import Control.Monad (zipWithM_)
import Data.List (intercalate)

--import Debug.Trace
--traceConstraint a b = trace ("[> " ++ cont a ++ " ==> " ++ cont b ++ " <]") (return ())
--  where cont a = show a ++ "{" ++ show (tv_id a) ++ "}"

type DS_TV = DisjointSet TypeVariable

unifyAll :: [Constraint] -> DS_TV ()
unifyAll = mapM_ unify

unify :: Constraint -> DS_TV ()
unify (ta, tb) = do
  ta' <- find ta
--  traceConstraint ta ta'
  tb' <- find tb
--  traceConstraint tb tb'
  unify' ta' tb'

unify' :: TypeVariable -> TypeVariable -> DS_TV ()
unify' ta@(TVInt _)       tb@(TVInt _)       = union ta tb
unify' ta@(TVRef t1 _)    tb@(TVRef t2 _)    = union ta tb >> unify (t1, t2)
unify' ta@(TVGenRef _)    tb@(TVRef _ _)     = union ta tb
unify' ta@(TVRef _ _)     tb@(TVGenRef _)    = union ta tb
unify' ta@(TVGenRef _)    tb@(TVGenRef _)    = union ta tb
unify' ta@(TVFun fa ra _) tb@(TVFun fb rb _) = union ta tb >> unifyIfEqual ra rb >> zipWithM_ unifyIfEqual fa fb
unify' ta@(TVVar _ _)     tb                 = union ta tb
unify' ta                 tb@(TVVar _ _)     = union ta tb
unify' ta                 tb                 = failedUnification ta tb

unifyIfEqual :: TypeVariable -> TypeVariable -> DS_TV ()
unifyIfEqual ta tb | ta `compat` tb  = unify (ta, tb)
                   | otherwise       = failedUnification ta tb

failedUnification :: TypeVariable -> TypeVariable -> a
failedUnification ta tb = error $ "Unable to unify " ++ show ta ++ " with " ++ show tb
