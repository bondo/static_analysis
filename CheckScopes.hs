module CheckScopes (checkScopes) where

import Ast (Program, Function(..), Stm(..), Id, i_val)

import Control.Monad (mapM_)
import Control.Monad.State
import Data.List
import qualified Data.Set as Set

-- Keep track of introduced identifiers
type IdSet = Set.Set Id
type IntroTracker = State IdSet

-- Return set of all identifiers
checkScopes :: Program -> IdSet
checkScopes p = execState (checkScopesProgram tln p) tln
  where tln = Set.fromList . checkFunctionList $ toplevelNames p

toplevelNames :: Program -> [Id]
toplevelNames = map f_name

checkScopesProgram :: IdSet -> Program -> IntroTracker ()
checkScopesProgram scope p = mapM_ (checkScopesFunction scope) p

checkScopesFunction :: IdSet -> Function -> IntroTracker ()
checkScopesFunction scope f = undefined

checkFunctionList :: [Id] -> [Id]
checkFunctionList l = if null dupes then l else err $ head dupes
  where dupes = filter ((>1) . fst) . map (\g -> (length g, head g)) . group $ sort l
        err g = error $ "The function " ++ i_val (snd g) ++ " is defined " ++ show (fst g) ++ " times"

check :: IdSet -> Id -> IntroTracker ()
check scope id = if id `Set.member` scope then error $
                 "The identifier " ++ i_val id ++ " cannot be shadowed"
                 else do intros <- get
                         if (id `Set.member` intros) 
                           then error $ "The identifier " ++ i_val id ++ " is used in two different scopes"
                           else put $ id `Set.insert` intros
