module Lattice (Lattice(..)) where

import Data.List
--import Data.Maybe
import Prelude hiding (pred, succ)

class Lattice e where
  leq :: e -> e -> Maybe Bool
  pred :: e -> [e]
  succ :: e -> [e]
  top :: e
  bottom :: e
  
  -- Ord instances are usefull for sorting, they will usually be
  -- derived and have no relation to leq
  leastUpperBound :: Ord e => e -> e -> e
  greatestLowerBound :: Ord e => e -> e -> e

  -- Default to transitive closure implementations
  leastUpperBound = leastUpperBound'
  greatestLowerBound = greatestLowerBound'

closure :: Ord e => (e -> [e]) -> e -> [e]
closure ite x = map head . group . sort . (x:) . concatMap (closure ite) $ ite x

intersection :: Ord e => [e] -> [e] -> [e]
intersection [] _ = []
intersection _ [] = []
intersection as@(a:as') bs@(b:bs') = case a `compare` b of
  LT -> intersection as' bs
  GT -> intersection as bs'
  EQ -> a : intersection as' bs'

closureIntersection :: Ord e => (e -> [e]) -> e -> e -> [e]
closureIntersection ite a b = intersection (closure ite a) (closure ite b)

leastUpperBound' :: (Ord e, Lattice e) => e -> e -> e
leastUpperBound' a b = foldl1' folder $ closureIntersection succ a b
  where folder a b = case a `leq` b of
          Just True  -> a
          Just False -> b
          Nothing    -> error "Lattice.LUB: Precondition failed"

greatestLowerBound' :: (Ord e, Lattice e) => e -> e -> e
greatestLowerBound' a b = foldl1' folder $ closureIntersection pred a b
  where folder a b = case a `leq` b of
          Just True  -> a
          Just False -> b
          Nothing    -> error "Lattice.GLB: Precondition failed"
