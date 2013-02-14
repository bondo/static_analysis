{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module DisjointSet where

import Control.Monad (liftM, unless)
import Control.Monad.State
import qualified Data.List as L
import Data.Set (Set)
import qualified Data.Set as Set

class Elem e where
  -- Choose which element to use as representative
  bestRep :: e -> e -> e
  bestRep a b = a
  
type EqClass e = (e, Set e)

type DisjointSet e = State [EqClass e]

instance (Show e, Ord e) => Show (DisjointSet e a) where
  show ds = result $ ds >> string

member :: Ord e => e -> EqClass e -> Bool
member a = Set.member a . snd

rep :: EqClass e -> e
rep = fst

set :: EqClass e -> Set e
set = snd

find :: Ord e => e -> DisjointSet e e
find a = findPure a >>= maybe (add a >> return a) return

findPure :: Ord e => e -> DisjointSet e (Maybe e)
findPure a = (maybe Nothing (Just . rep) . L.find (member a)) `liftM` get

contains :: Ord e => e -> DisjointSet e Bool
contains a = any (member a) `liftM` get

union :: (Elem e, Ord e) => e -> e -> DisjointSet e ()
union a b = do a' <- find a
               b' <- find b
               unless (a' == b') $ union' a' b'
  where union' a b = do ca <- takeClassWith a
                        cb <- takeClassWith b
                        putClass (bestRep a b, set ca `Set.union` set cb)

add :: Ord e => e -> DisjointSet e ()
add a = contains a >>= \old -> unless old $ putClass $ (a, Set.singleton a)

empty :: DisjointSet e ()
empty = return ()

toList :: Ord e => DisjointSet e a -> [[e]]
toList ds = map ecToList $ execState ds []

ecToList :: Ord e => EqClass e -> [e]
ecToList ec = [rep ec] ++ (Set.toList $ (rep ec `Set.delete` set ec))

string :: (Show e, Ord e) => DisjointSet e String
string = (L.intercalate "\n" . map (L.intercalate " == " . map show . ecToList)) `liftM` get

result :: DisjointSet e a -> a
result ds = evalState ds []

-- Precondition: all (\a -> contains a == False) as
putClass :: Ord e => EqClass e -> DisjointSet e ()
putClass as = state $ \ass -> ((), as:ass)

-- Precondition: contains a == True
takeClassWith :: Ord e => e -> DisjointSet e (EqClass e)
takeClassWith a = get >>= (\(as, b:bs) -> put (as++bs) >> return b) . break (member a)
