module Cfg (cfgFromProgram, Cfg(..), CfgNode(..)) where

import Ast

import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as UidMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as UidSet

type Uid = Int
type UidSet = IntSet
type UidMap = IntMap

data Cfg = Cfg { cEntry :: CfgNode, cExit :: CfgNode }

data CfgNode = CAss    { cStm :: Stm,
                         cPred :: [CfgNode], cSucc :: [CfgNode], cUid :: Uid }
             | COutput { cExpr :: Expr,
                         cPred :: [CfgNode], cSucc :: [CfgNode], cUid :: Uid }
             | CBranch { cExpr :: Expr, cTrue :: CfgNode, cFalse :: CfgNode, cStm :: Stm,
                         cPred :: [CfgNode], cSucc :: [CfgNode], cUid :: Uid }
             | CDecl   { cIds :: [Id],
                         cPred :: [CfgNode], cSucc :: [CfgNode], cUid :: Uid }
             | CNop    { cPred :: [CfgNode], cSucc :: [CfgNode], cUid :: Uid }

data CfgGenNode = GNAss    { gnStm :: Stm,
                             gnPred :: UidSet, gnSucc :: UidSet }
                | GNOutput { gnExpr :: Expr,
                             gnPred :: UidSet, gnSucc :: UidSet }
                | GNBranch { gnExpr :: Expr, gnStm :: Stm, gnTrue :: Uid, gnFalse :: Uid,
                             gnPred :: UidSet, gnSucc :: UidSet }
                | GNDecl   { gnIds :: [Id],
                             gnPred :: UidSet, gnSucc :: UidSet }
                | GNNop    { gnPred :: UidSet, gnSucc :: UidSet }

type UidState = State Uid
type CfgGen = StateT (UidMap CfgGenNode) UidState


-- Utility functions for handling state

nuid :: CfgGen Uid
nuid = lift $ modify (+1) >> get

modifyNodes :: (UidMap CfgGenNode -> UidMap CfgGenNode) -> CfgGen ()
modifyNodes f = modify f

modifyNode :: (CfgGenNode -> CfgGenNode) -> Uid -> CfgGen ()
modifyNode f = modifyNodes . UidMap.adjust f

setNode :: Uid -> CfgGenNode -> CfgGen ()
setNode uid = modifyNodes . UidMap.insert uid

-- Precondition: uid in map
getNode :: Uid -> CfgGen CfgGenNode
getNode uid = (UidMap.! uid) `liftM` get

-- Precondition: uid in map
getsNode :: (CfgGenNode -> a) -> Uid -> CfgGen a
getsNode f uid = f `liftM` getNode uid

-- Precondition: uid in map
getPreds :: Uid -> CfgGen [Uid]
getPreds = getsNode $ UidSet.toList . gnPred

-- Precondition: uid in map
getSuccs :: Uid -> CfgGen [Uid]
getSuccs = getsNode $ UidSet.toList . gnSucc

addPred :: Uid -> Uid -> CfgGen ()
addPred pred = modifyNode $ \n -> n { gnPred = UidSet.insert pred $ gnPred n }

addSucc :: Uid -> Uid -> CfgGen ()
addSucc succ = modifyNode $ \n -> n { gnSucc = UidSet.insert succ $ gnSucc n }

setPreds :: [Uid] -> Uid -> CfgGen ()
setPreds preds = modifyNode $ \n -> n { gnPred = UidSet.fromList preds }

setSuccs :: [Uid] -> Uid -> CfgGen ()
setSuccs succs = modifyNode $ \n -> n { gnSucc = UidSet.fromList succs }


-- Utility functions for handling CFG nodes

connect :: [Uid] -> [Uid] -> CfgGen ()
connect ps ss = forM_ ps $ \p -> forM_ ss $ \s -> addPred p s >> addSucc s p

nop :: CfgGen Uid
nop = do uid <- nuid
         setNode uid $ GNNop UidSet.empty UidSet.empty
         return uid

chain :: (Uid, Uid) -> (Uid, Uid) -> CfgGen (Uid, Uid)
chain (p1, s1) (p2, s2) = do s1ps <- getPreds s1
                             p2ss <- getSuccs p2
                             mapM_ (setPreds s1ps) p2ss
                             mapM_ (setSuccs p2ss) s1ps
                             return (p1, s2)

create :: (UidSet -> UidSet -> CfgGenNode) -> CfgGen (Uid, Uid)
create con = do p <- nop
                s <- nop
                n <- nuid
                setNode n $ con UidSet.empty UidSet.empty
                connect [p] [n]
                connect [n] [s]
                return (p, s)


-- Functions that return finished CFGs

cfgFromProgram :: Program -> Cfg
cfgFromProgram = undefined

fromProgram :: Program -> CfgGen Cfg
fromProgram = undefined

fromFunction :: Function -> CfgGen Cfg
fromFunction = undefined


-- Functions that build intermediate CFG representation

fromStm :: Stm -> CfgGen (Uid, Uid)
fromStm s@SAss{}        = create $ GNAss s
fromStm s@SAssRef{}     = create $ GNAss s
fromStm (SOutput e)     = create $ GNOutput e
fromStm (SSeq ss@(_:_)) = mapM fromStm ss >>= \(s':ss') -> foldM chain s' ss'
fromStm (SSeq _)        = error "Empty sequence of statements not weeded out"
fromStm s@(SIfElse cond th el) = do (tp, ts) <- fromStm th
                                    (ep, es) <- fromStm el
                                    np <- nop
                                    ns <- nop
                                    uid <- nuid
                                    setNode uid $ GNBranch cond s tp ep UidSet.empty UidSet.empty
                                    connect [np] [uid]
                                    connect [uid] [tp, ep]
                                    connect [ts, es] [ns]
                                    return (np, ns)
fromStm s@(SWhile cond body) = do (bp, bs) <- fromStm body
                                  np <- nop
                                  ns <- nop
                                  uid <- nuid
                                  setNode uid $ GNBranch cond s bp bs UidSet.empty UidSet.empty
                                  connect [np, bs] [uid]
                                  connect [uid] [ns, bp]
                                  return (np, ns)
fromStm (SDecl ids) = create $ GNDecl ids
fromStm (SReturn _) = error $ "Return statement not weeded out"
fromStm SNop = create GNNop

