module Weeder (weed) where

import Ast

import Control.Monad.Instances -- Instance for (Monad (Either String))

{-
  TODO:
    * make sure no SReturn exists
    * make sure identifiers are unique
-}

weed :: Program -> Either String Program
weed = sequence . map cleanFunction

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