{-# LANGUAGE TupleSections #-}

module ProductLattice where

import Lattice

import Prelude hiding (pred, succ)

instance (Lattice a, Lattice b) => Lattice (a, b) where
  (a1, a2) `leq` (b1, b2) = if r1 == r2 then r1 else Nothing
    where r1 = a1 `leq` b1
          r2 = a2 `leq` b2
  pred (a, b) = map (,b) (pred a) ++ map (a,) (pred b)
  succ (a, b) = map (,b) (succ a) ++ map (a,) (succ b)
  top (a, b) = (top a, top b)
  bottom (a, b) = (bottom a, bottom b)

instance (Lattice a, Lattice b, Lattice c) => Lattice (a, b, c) where
  (a1, a2, a3) `leq` (b1, b2, b3) = if r1 == r2 && r2 == r3 then r1 else Nothing
    where r1 = a1 `leq` b1
          r2 = a2 `leq` b2
          r3 = a3 `leq` b3
  pred (a, b, c) = map (,b,c) (pred a) ++ map (a,,c) (pred b) ++ map (a,b,) (pred c)
  succ (a, b, c) = map (,b,c) (succ a) ++ map (a,,c) (succ b) ++ map (a,b,) (succ c)
  top (a, b, c) = (top a, top b, top c)
  bottom (a, b, c) = (bottom a, bottom b, bottom c)
