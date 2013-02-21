module MapLattice () where

import Lattice

import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (pred, succ)

instance (Ord k, Lattice v) => Lattice (Map k v) where
  a `leq` b = if Map.null a || Map.keys a /= Map.keys b
              then Nothing
              else
                let (v:vs) = zipWith leq (Map.elems a) (Map.elems b) in
                if all (v==) vs then v else Nothing
  pred = concatAdjust pred
  succ = concatAdjust succ
  top = Map.map top
  bottom = Map.map bottom

concatAdjust :: (Ord k, Lattice v) => (v -> [v]) -> Map k v -> [Map k v]
concatAdjust ite m =
  concatMap (\(k,v) -> [ Map.adjust (const p) k m | p <- pred v ]) $ Map.assocs m
