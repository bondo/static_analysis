module Cfg (cfgFromProgram) where

import Ast

import Control.Monad (mapM_)
import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as Map
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set

type Uid = Int

data Cfg = Cfg { c_entry :: CfgNode, c_exit :: CfgNode }

data CfgNode = CAss    { c_stm :: Stm,
                         c_pred :: [Cfg], c_succ :: [Cfg], c_uid :: Uid }
             | COutput { c_expr :: Expr,
                         c_pred :: [Cfg], c_succ :: [Cfg], c_uid :: Uid }
             | CBranch { c_expr :: Expr, c_true :: CfgNode, c_false :: CfgNode, c_stm :: Stm,
                         c_pred :: [Cfg], c_succ :: [Cfg], c_uid :: Uid }
             | CDecl   { c_ids :: [Id],
                         c_pred :: [Cfg], c_succ :: [Cfg], c_uid :: Uid }
             | CNop    { c_pred :: [Cfg], c_succ :: [Cfg], c_uid :: Uid }

data CfgGenNode = CfgGenNode { gn_node :: CfgNode, gn_pred :: IntSet, gn_succ :: IntSet }
data CfgGenState = CfgGenState { gs_nuid :: Uid, gs_nodes :: IntMap CfgGenNode }
type CfgGen = State CfgGenState

getPreds :: Uid -> CfgGen [Uid]
getPreds = undefined

getSuccs :: Uid -> CfgGen [Uid]
getSuccs = undefined

addPred :: Uid -> Uid -> CfgGen ()
addPred p n = undefined

addSucc :: Uid -> Uid -> CfgGen ()
addSucc s n = undefined

setPreds :: [Uid] -> Uid -> CfgGen ()
setPreds ps n = undefined

setSuccs :: [Uid] -> Uid -> CfgGen ()
setSuccs ss n = undefined

addNode :: CfgNode -> CfgGen ()
addNode = undefined

cfgFromProgram :: Program -> Cfg
cfgFromProgram = undefined

fromProgram :: Program -> CfgGen Cfg
fromProgram = undefined

fromFunction :: Function -> CfgGen Cfg
fromFunction = undefined

nuid :: CfgGen Uid
nuid = undefined

nop :: CfgGen Uid
nop = undefined

chain :: (Uid, Uid) -> (Uid, Uid) -> CfgGen (Uid, Uid)
chain (p1, s1) (p2, s2) = do s1ps <- getPreds s1
                             p2ss <- getSuccs p2
                             mapM_ (setPreds s1ps) p2ss
                             mapM_ (setSuccs p2ss) s1ps
                             return (p1, s2)

create :: ([Cfg] -> [Cfg] -> Uid -> CfgNode) -> CfgGen (Uid, Uid)
create con = do p <- nop
                s <- nop
                n <- nuid
                addNode $ con [] [] n
                addPred p n
                addSucc s n
                addSucc n p
                addPred n s
                return (p, s)

fromStm :: Stm -> CfgGen (Uid, Uid)
fromStm s@SAss{} = undefined




