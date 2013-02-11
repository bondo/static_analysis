module Weeder (weed) where

import Ast (Program, Function(..), Stm(..), i_val)

import Control.Monad.Instances -- Instance for (Monad (Either String))
import Data.Generics.Aliases (orElse)
import Data.Maybe (mapMaybe, listToMaybe)

weed :: Program -> Either String Program
weed = mapM $ \f -> do
  f' <- cleanFunction f
  maybe (Right f') Left $ checkReturn f'

missingReturn = "Return statement missing at the end of "

-- Convert FNamedSimple to FNamed and "compress" statements
cleanFunction :: Function -> Either String Function
cleanFunction (FNamedSimple name _ (SSeq []))       = Left $ missingReturn ++ i_val name
cleanFunction (FNamedSimple name formals (SSeq ss)) = case last ss of
  SReturn ret -> Right $ FNamed name formals (compressStm . SSeq $ init ss) ret
  _           -> Left  $ missingReturn ++ i_val name
cleanFunction (FNamedSimple name formals (SReturn expr)) = Right $ FNamed name formals SNop expr
cleanFunction (FNamedSimple name _ _)                    = Left  $ missingReturn ++ i_val name
cleanFunction (FNamed n f s r)                           = Right $ FNamed n f (compressStm s) r

-- The parser emits a lot more SSeq then it should, so clean it up
compressStm :: Stm -> Stm
compressStm (SSeq [])         = SNop
compressStm (SSeq [s])        = compressStm s
compressStm (SSeq ss)         = SSeq $ map compressStm ss
compressStm (SIfElse e s1 s2) = SIfElse e (compressStm s1) (compressStm s2)
compressStm (SWhile e s)      = SWhile e $ compressStm s
compressStm s                 = s


-- Check to make sure no more SReturn statements exists
checkReturn :: Function -> Maybe String
checkReturn f = annotate . checkReturn' $ f_body f
  where annotate (Just msg) = Just $ "Error in " ++ i_val (f_name f) ++ ": " ++ msg
        annotate Nothing    = Nothing

checkReturn' :: Stm -> Maybe String
checkReturn' s@(SSeq ss)       = listToMaybe $ mapMaybe checkReturn' ss
checkReturn' s@(SIfElse _ t e) = checkReturn' t `orElse` checkReturn' e
checkReturn' s@(SWhile _ b)    = checkReturn' b
checkReturn' (SReturn _)       = Just "Return statements are only allowed as the last statement in a function"
checkReturn' s                 = Nothing
