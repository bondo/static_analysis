{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module DisjointSet where

import Control.Monad (liftM, unless)
import Control.Monad.State
import qualified Data.List as L

class Elem e where
  -- Choose which element to use as representative
  bestRep :: e -> e -> e
  bestRep a b = a

type DisjointSet e = State [[e]]

instance Show e => Show (DisjointSet e a) where
  show ds = result $ ds >> string

find :: (Elem e, Eq e) => e -> DisjointSet e e
find a = findPure a >>= maybe (add a >> return a) return

findPure :: (Elem e, Eq e) => e -> DisjointSet e (Maybe e)
findPure a = (maybe Nothing (Just . head) . L.find (elem a)) `liftM` get

contains :: (Elem e, Eq e) => e -> DisjointSet e Bool
contains a = any (elem a) `liftM` get

union :: (Elem e, Eq e) => e -> e -> DisjointSet e ()
union a b = do a' <- find a
               b' <- find b
               unless (a' == b') $ select a' b'
  where select a b = if a == bestRep a b
                     then union' a b
                     else union' b a
        union' a b = do ca <- takeClassWith a
                        cb <- takeClassWith b
                        putClass (ca ++ cb)

add :: (Elem e, Eq e) => e -> DisjointSet e ()
add a = contains a >>= \old -> unless old $ putClass [a]

empty :: DisjointSet e ()
empty = return ()

toList :: DisjointSet e a -> [[e]]
toList ds = execState ds []

-- Preconditions: list created by toList
fromList :: [[e]] -> DisjointSet e ()
fromList lst = empty >> put lst

string :: Show e => DisjointSet e String
string = (L.intercalate "\n" . map (L.intercalate " == " . map show)) `liftM` get

result :: DisjointSet e a -> a
result ds = evalState ds []

-- Precondition: all (\a -> contains a == False) as
putClass :: Elem e => [e] -> DisjointSet e ()
putClass as = state $ \ass -> ((), as:ass)

-- Precondition: contains a == True
takeClassWith :: (Elem e, Eq e) => e -> DisjointSet e [e]
takeClassWith a = get >>= (\(as, b:bs) -> put (as++bs) >> return b) . break (elem a)
