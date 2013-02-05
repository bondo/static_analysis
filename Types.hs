module Types where

type Type = TInt
          | TRef Type
          | TFun [Type] Type
          deriving (Show, Eq)
