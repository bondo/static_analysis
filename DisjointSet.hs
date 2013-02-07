{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module DisjointSet where

import Control.Monad.State
import qualified Data.List as L

class Elem e where
  -- Choose which element to use as representative
  bestRep :: e -> e -> e
  bestRep a b = a

type DisjointSet e = State [[e]]

instance Show e => Show (DisjointSet e a) where
  show ds = "DisjointSet{" ++ show (map (map show) $ toList ds) ++ "}"

find :: (Elem e, Eq e) => e -> DisjointSet e e
find a = get >>= maybe (add a >> return a) (return . head) . L.find (any (==a))

findPure :: (Elem e, Eq e) => e -> DisjointSet e (Maybe e)
findPure a = get >>= maybe (return Nothing) (return . Just . head) . L.find (any (==a))

contains :: (Elem e, Eq e) => e -> DisjointSet e Bool
contains a = get >>= return . any (any (==a))

union :: (Elem e, Eq e) => e -> e -> DisjointSet e ()
union a b = do a' <- find a
               b' <- find b
               if a' == b' then return () else select a' b'
  where select a b = if a == (bestRep a b)
                     then union' a b
                     else union' b a
        union' a b = do ca <- takeClassWith a
                        cb <- takeClassWith b
                        putClass (ca ++ cb)

add :: (Elem e, Eq e) => e -> DisjointSet e ()
add a = contains a >>= \old -> if old then return () else putClass [a]

empty :: DisjointSet e ()
empty = return ()

toList :: DisjointSet e a -> [[e]]
toList ds = execState ds []

result :: DisjointSet e a -> a
result ds = evalState ds []

-- Precondition: all (\a -> contains a == False) as
putClass :: Elem e => [e] -> DisjointSet e ()
putClass as = state $ \ass -> ((), as:ass)

-- Precondition: contains a == True
takeClassWith :: (Elem e, Eq e) => e -> DisjointSet e [e]
takeClassWith a = get >>= (\(as, b:bs) -> put (as++bs) >> return b) . break (any (==a))
