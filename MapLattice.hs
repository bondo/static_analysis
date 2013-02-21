module MapLattice (MapLattice(..)) where

import Lattice

import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (pred, succ)

newtype MapLattice k v = MapLattice (Map k v) deriving (Eq, Ord)

instance (Ord k, Lattice v) => Lattice (MapLattice k v) where
  MapLattice a `leq` MapLattice b =
    if Map.null a || Map.keys a /= Map.keys b
    then Nothing
    else
      let (v:vs) = zipWith leq (Map.elems a) (Map.elems b) in
      if all (v==) vs then v else Nothing
  pred = concatAdjust pred
  succ = concatAdjust succ
  top (MapLattice m) = MapLattice $ Map.map top m
  bottom (MapLattice m) = MapLattice $ Map.map bottom m

concatAdjust :: (Ord k, Lattice v) => (v -> [v]) -> MapLattice k v -> [MapLattice k v]
concatAdjust ite (MapLattice m) =
  concatMap (\(k,v) -> [ MapLattice $ Map.adjust (const p) k m | p <- pred v ]) $ Map.assocs m