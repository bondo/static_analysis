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

modifyNodes :: (UidMap CfgGenNode -> UidMap CfgGenNode) -> CfgGen ()
modifyNodes f = modify $ \st -> st { gsNodes = f $ gsNodes st }

-- Precondition: uid is in map
modifyNode :: Uid -> (CfgGenNode -> CfgGenNode) -> CfgGen ()
modifyNode uid f = modifyNodes $ \ns -> UidMap.insert uid (f $ ns UidMap.! uid) ns

-- Precondition: uid is in map
getNode :: Uid -> CfgGen CfgGenNode
getNode uid = (UidMap.! uid) `liftM` gets gsNodes

setNode :: Uid -> CfgGenNode -> CfgGen ()
setNode uid n = modify $ \st -> st { gsNodes = UidMap.insert uid n $ gsNodes st }

-- Precondition: uid is in map
getPreds :: Uid -> CfgGen [Uid]
getPreds uid = (UidSet.toList . gnPred) `liftM` getNode uid

-- Precondition: uid is in map
getSuccs :: Uid -> CfgGen [Uid]
getSuccs uid = (UidSet.toList . gnSucc) `liftM` getNode uid

-- Precondition: n is in map
addPred :: Uid -> Uid -> CfgGen ()
addPred p n = undefined

-- Precondition: n is in map
addSucc :: Uid -> Uid -> CfgGen ()
addSucc s n = undefined

-- Precondition: n is in map
setPreds :: [Uid] -> Uid -> CfgGen ()
setPreds ps n = undefined

-- Precondition: n is in map
setSuccs :: [Uid] -> Uid -> CfgGen ()
setSuccs ss n = undefined

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

fromStm :: Stm -> CfgGen (Uid, Uid)
fromStm s@SAss{} = undefined




