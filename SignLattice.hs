module SignLattice where

import Lattice

import Prelude hiding ((/))

data SignLattice = Bottom | Zero | Minus | Plus | Top deriving (Eq, Ord, Show)

instance Lattice SignLattice where
  leq _ Top = Just True
  leq Top _ = Just False
  leq Bottom _ = Just True
  leq _ Bottom = Just False
  leq _ _ = Nothing

  pred Bottom = []
  pred Zero = [Bottom]
  pred Minus = [Bottom]
  pred Plus = [Bottom]
  pred Top = [Plus, Minus, Zero]

  succ Bottom = [Plus, Minus, Zero]
  succ Zero = [Top]
  succ Minus = [Top]
  succ Plus = [Top]
  succ Top = []

  top = Top

  bottom = Bottom

instance Num SignLattice where
  Bottom + _      = Bottom
  _      + Bottom = Bottom
  Zero   + b      = b
  a      + Zero   = a
  Minus  + Minus  = Minus
  Plus   + Plus   = Plus
  _      + _      = Top

  Bottom * _      = Bottom
  _      * Bottom = Bottom
  Zero   * _      = Zero
  _      * Zero   = Zero
  Minus  * Minus  = Plus
  Plus   * Plus   = Plus
  Minus  * Plus   = Minus
  Plus   * Minus  = Minus
  _      * _      = Top

  Bottom - _ = Bottom
  _ - Bottom = Bottom
  Zero - b = negate b
  a - Zero = a
  Plus - Minus = Plus
  Minus - Plus = Minus
  _ - _ = Top

  negate Plus = Minus
  negate Minus = Plus
  negate a = a

  abs = error "SignLattice.abs not implemented"

  signum = error "SignLattice.signum not implemented"

  fromInteger 0 = Zero
  fromInteger a = if a < 0 then Minus else Plus

Bottom / _      = Bottom
_      / Bottom = Bottom
Zero   / Minus  = Zero
Zero   / Plus   = Zero
_      / _      = Top
