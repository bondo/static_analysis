module Types where

data Type = TInt
          | TRef Type
          | TFun [Type] Type
          deriving (Show, Eq)
