module Cfg (cfgFromProgram, Cfg(..), CfgNode(..)) where

import Ast

import Control.Monad (mapM_, liftM)
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

data CfgGenState = CfgGenState { gsNuid :: Uid, gsNodes :: UidMap CfgGenNode }

type CfgGen = State CfgGenState


-- Utility functions for handling state

nuid :: CfgGen Uid
nuid = state $ \st -> (gsNuid st, st { gsNuid = gsNuid st + 1 })

modifyNodes :: (UidMap CfgGenNode -> UidMap CfgGenNode) -> CfgGen ()
modifyNodes f = modify $ \st -> st { gsNodes = f $ gsNodes st }

modifyNode :: (CfgGenNode -> CfgGenNode) -> Uid -> CfgGen ()
modifyNode f = modifyNodes . UidMap.adjust f

setNode :: Uid -> CfgGenNode -> CfgGen ()
setNode uid = modifyNodes . UidMap.insert uid

-- Precondition: uid in map
getNode :: Uid -> CfgGen CfgGenNode
getNode uid = (UidMap.! uid) `liftM` gets gsNodes

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
                addPred p n
                addSucc s n
                addSucc n p
                addPred n s
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
fromStm s@SAss{} = undefined
