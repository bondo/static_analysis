module Weeder (weed) where

import Ast

import Control.Monad.Instances -- Instance for (Monad (Either String))

{-
  TODO:
    * make sure no SReturn exists
    * make sure identifiers are unique
  DONE:
    * Convert FNamed to FNamedReturn
    * "compress" statements
-}

weed :: Program -> Either String Program
weed = sequence . map cleanFunction

missingReturn = "Return statement missing at the end of "

-- Do the conversion and compression
cleanFunction :: Function -> Either String Function
cleanFunction (FNamed name _ (SSeq []))       = Left $ missingReturn ++ name
cleanFunction (FNamed name formals (SSeq ss)) =
  case last ss of
    SReturn ret -> Right $ FNamedReturn name formals (compressStm . SSeq $ init ss) ret
    _           -> Left  $ missingReturn ++ name
cleanFunction (FNamed name formals (SReturn expr)) = Right $ FNamedReturn name formals SNop expr
cleanFunction (FNamed name _ _)                    = Left $ missingReturn ++ name
cleanFunction (FNamedReturn n f s r)               = Right $ FNamedReturn n f (compressStm s) r

compressStm :: Stm -> Stm
compressStm (SSeq [])         = SNop
compressStm (SSeq [s])        = compressStm s
compressStm (SSeq ss)         = SSeq $ map compressStm ss
compressStm (SIfElse e s1 s2) = SIfElse e (compressStm s1) (compressStm s2)
compressStm (SWhile e s)      = SWhile e $ compressStm s
compressStm s                 = s