module Types where

import Control.DeepSeq (NFData(rnf))
import Data.List (intercalate)

data Type = TInt
          | TRef Type
          | TFun [Type] Type
          | TReg Type -- Regular type, where the argument is a parent reference

parens :: String -> String
parens a = "(" ++ a ++ ")"

showFun :: Int -> [Type] -> Type -> String
showFun n dom cod | null dom = "() -> " ++ show' n cod
showFun n dom cod = "(" ++ intercalate ", " (map (show' n) dom) ++ ") -> " ++ show' n cod

instance Eq Type where
  TInt        == TInt         = True
  TRef t1     == TRef t2      = t1 == t2
  TFun d1 c1  == TFun d2 c2   = c1 == c2 && all id (zipWith (==) d1 d2)
--TReg t1     == TReg t2      = ???
  _           == _            = False

instance Show Type where
  show = show' 2

instance NFData Type where
  rnf TInt        = ()
  rnf (TRef t)    = rnf t
  rnf (TFun ts t) = rnf $ t : ts
  rnf (TReg t)    = rnf t

show' n TInt           = "int"
show' n (TRef t)       = '&' : show' n t
show' n (TFun dom cod) = showFun n dom cod
show' 0 (TReg _)       = "..."
show' n (TReg t)       = show' (n-1) t
