module Types where

import Data.List (intercalate)

data Type = TInt
          | TRef Type
          | TFun [Type] Type
          | TGen -- generic, i.e. \alpha

parens a = "(" ++ a ++ ")"

showFun dom cod = intercalate " -> " (map showFunParens dom) ++ " -> " ++ show cod
  where showFunParens f@(TFun _ _) = parens $ show f
        showFunParens t            = show t


instance Eq Type where
  TInt        == TInt         = True
  TRef t1     == TRef t2      = t1 == t2
  TFun d1 c1  == TFun d2 c2   = c1 == c2 && (all id $ zipWith (==) d1 d2)
  TGen        == _            = True
  _           == TGen         = True
  _           == _            = False

instance Show Type where
  show TInt           = "Int"
  show (TRef t)       = "&" ++ show t
  show (TFun dom cod) = showFun dom cod
  show TGen           = "a"
