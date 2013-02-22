module CheckScopes (checkScopes) where

import Ast (Program, Function(..), Stm(..), Expr(..), Id, iVal)

import Control.Arrow ((&&&))
import Control.Monad (mapM_, unless, foldM)
import Control.Monad.State
import Data.List
import qualified Data.Set as Set

-- Keep track of introduced identifiers
type IdSet = Set.Set Id
type IntroTracker = State IdSet

-- Return set of all identifiers
checkScopes :: Program -> IdSet
checkScopes p = execState (checkProgram fs p) fs
  where fs = Set.fromList $ checkFunctionNames p

checkFunctionNames :: Program -> [Id]
checkFunctionNames p = if null dupes then names else err $ head dupes
  where dupes = filter ((>1) . fst) . map (length &&& head) . group $ sort names
        err g = error $ "The function " ++ iVal (snd g) ++ " is defined " ++ show (fst g) ++ " times"
        names = map fName p

checkProgram :: IdSet -> Program -> IntroTracker ()
checkProgram scope = mapM_ $ checkFunction scope

checkFunction :: IdSet -> Function -> IntroTracker ()
checkFunction scope f@FNamed{} = do scope' <- foldM intro scope $ fFormals f
                                    scope'' <- checkStm scope' $ fBody f
                                    checkExpr scope'' $ fRetval f
checkFunction _ _ = error "Scope checking only implemented for weeded functions"

checkStm :: IdSet -> Stm -> IntroTracker IdSet
checkStm s (SAss name val)        = checkExpr s val >> check s name >> return s
checkStm s (SAssRef name val)     = checkExpr s val >> check s name >> return s
checkStm s (SOutput val)          = checkExpr s val >> return s
checkStm s (SSeq stms)            = foldM checkStm s stms
checkStm s (SIfElse cond thn els) = checkExpr s cond >> checkStm s thn >> checkStm s els >> return s
checkStm s (SWhile cond body)     = checkExpr s cond >> checkStm s body >> return s
checkStm s (SDecl ids)            = foldM intro s ids
checkStm s (SReturn val)          = checkExpr s val >> return s
checkStm s SNop                   = return s

checkExpr :: IdSet -> Expr -> IntroTracker ()
checkExpr s (EConst _ _)              = return ()
checkExpr s (EVar name)               = check s name
checkExpr s (EBinOp _ l r _)          = checkExpr s l >> checkExpr s r
checkExpr s (EAppNamed name args _)   = check s name >> mapM_ (checkExpr s) args
checkExpr s (EAppUnnamed expr args _) = mapM_ (checkExpr s) $ expr : args
checkExpr s (ERef name _)             = check s name
checkExpr s (EDeRef expr _)           = checkExpr s expr
checkExpr s (EInput _)                = return ()
checkExpr s (EMalloc _)               = return ()
checkExpr s (ENull _)                 = return ()

intro :: IdSet -> Id -> IntroTracker IdSet
intro scope id = if id `Set.member` scope then error $
                 "The identifier " ++ iVal id ++ " cannot be shadowed"
                 else do intros <- get
                         if id `Set.member` intros
                           then error $ "The identifier " ++ iVal id ++ " is used in two different scopes"
                           else put (id `Set.insert` intros) >> return (id `Set.insert` scope)

check :: IdSet -> Id -> IntroTracker ()
check scope id = unless (id `Set.member` scope) $ error $
                 "The identifier " ++ iVal id ++ " is used out of scope"
