{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}

module Lang.Primitives where

import qualified Data.Map as Map
import Lang.Syntax

type0 :: Type 1
type0 = tyCon1 "Type"

type1 :: Type 2
type1 = tyCon2 "Type"

natTy :: Type 0
natTy = tyCon0 "Nat"

floatTy :: Type 0 -> Type 0
floatTy t = TyApp (ImplicitTyApp (tyCon0 "Float") (tyCon1 "Base")) [t]

integerTy :: Type 0 -> Type 0
integerTy t = TyApp (ImplicitTyApp (tyCon0 "Integer") (tyCon1 "Base")) [t]

stringTy :: Type 0 -> Type 0
stringTy t = TyApp (ImplicitTyApp (tyCon0 "String") (tyCon1 "Base")) [t]

boolTy :: Type 0 -> Type 0
boolTy t = TyApp (ImplicitTyApp (tyCon0 "Bool") (tyCon1 "Base")) [t]

-- | Smart constructors for the descriptors of graded types
unitTy, dimensionTy, quantityTy, speciesTy, basisTy :: Type 0 -> Type 0
unitTy t      = TyApp (tyCon0 "Unit") [t]
dimensionTy t = TyApp (tyCon0 "Dimension") [t]
quantityTy t  = TyApp (tyCon0 "Quantity") [t]
speciesTy t   = TyApp (tyCon0 "Species") [t]
basisTy t     = TyApp (tyCon0 "Basis") [t]

-- | The quantity of angles
angleQuantity :: Type 0
angleQuantity = quantityTy (tyCon0 "Angle")

-- | Affine-space descriptors: the names for the weights 1, 0 and -1, the
-- general form Affine[weight], and a scalar of known value Scalar[c]
pointTy, vectorTy, npointTy :: Type 0
pointTy  = tyCon0 "Point"
vectorTy = tyCon0 "Vector"
npointTy = tyCon0 "NPoint"

affineTy, scalarTy :: Rational -> Type 0
affineTy w = TyApp (tyCon0 "Affine") [TyRat w]
scalarTy c = TyApp (tyCon0 "Scalar") [TyRat c]

desc :: Type 1
desc = tyCon1 "Descriptor"

desc2 :: Type 2
desc2 = tyCon2 "Descriptor"

-- TODO:
-- prims :: [(Identifier, Type 0)]
-- prims = [
--   ("sqrt", ImpliftFunTy "d" desc2 (ImplicitFunTy "u"
--              FunTY (TyApp (ImplicitTyApp (tyCon0 "Float") (TyVar d)) (TyVar "u"))
--                    (TyApp (ImplicitTyApp (tyCon0 "Float") (TyVar d)) (ExponentTy (TyVar "u") 0.5)

-- ]

-- | The kind of type-level rationals such as the weight in Affine[1/2]
ratKind :: Type 1
ratKind = tyCon1 "Rational"

dataConstructors :: [(Identifier, Type 0)]
dataConstructors = [
    ("None"     , tyCon0 "None")
  , ("True"     , boolTy (tyCon0 "1"))
  , ("False"    , boolTy (tyCon0 "1"))
 ]

-- | The descriptor kinds, each paired with the type constructor that forms
-- its descriptors from a field of the given kind, e.g. Unit[m] : UoM from
-- m : UoMBase. The constructor also names the kind's component in the
-- representation of a description.
descriptorKinds :: [(Identifier, (Identifier, Type 1))]
descriptorKinds =
  [ ("UoM",         ("Unit",      unitBase))
  , ("KoQ",         ("Quantity",  type0))
  , ("SpeciesType", ("Species",   type0))
  , ("BasisType",   ("Basis",     type0)) -- TODO: type0 should be the open type Bases here
  , ("Dim",         ("Dimension", dimensionBase))
  , ("AffineSpace", ("Affine",    ratKind)) -- Affine[weight]
  ]

typeConstructors :: [(Identifier, Type 1)]
typeConstructors =
  -- the constructors of each descriptor kind from its field
  [ (con, FunTy [field] (tyCon1 kind)) | (kind, (con, field)) <- descriptorKinds ]
  ++ [
     -- Graded float
    ("Float"    , ImplicitFunTy "d" desc2 (FunTy [tyVar "d"] type0))
     -- Graded integer
  , ("Integer"  , ImplicitFunTy "d" desc2 (FunTy [tyVar "d"] type0))
     -- Graded string
  , ("String"  , ImplicitFunTy "d" desc2 (FunTy [tyVar "d"] type0))
    -- Graded boolean
  , ("Bool"     , ImplicitFunTy "d" desc2 (FunTy [tyVar "d"] type0))
  , ("Nat"      , type0)
  -------
    -- the other constructors of affine-space descriptors
  , ("Point",     tyCon1 "AffineSpace")
  , ("Vector",    tyCon1 "AffineSpace")
  , ("NPoint",    tyCon1 "AffineSpace")
  , ("Scalar",    FunTy [ratKind] (tyCon1 "AffineSpace")) -- Scalar[value]
  , ("None"     , type0)
    -- The standard 7 SI base dimensions
  , ("T"        , dimensionBase) -- Time
  , ("L"        , dimensionBase) -- Length
  , ("M"        , dimensionBase) -- Mass
  , ("Theta"      , dimensionBase) -- Thermodynamic temperature (Theta)
  , ("N"        , dimensionBase) -- Amount of substance
  , ("U"        , dimensionBase) -- Electric cUrrent
  , ("Ji"        , dimensionBase) -- Luminous intensity -- TODO: allow overloaded between J for dim and for joules
    -- The standard 7 SI units of measure
  , ("s"        , unitBase) -- seconds
  , ("m"        , unitBase) -- metres
      , ("km"        , unitBase) -- kilometres [for demonstration]
  , ("kg"       , unitBase) -- kilograms
  , ("K"        , unitBase) -- Kelvin
  , ("mol"      , unitBase) -- moles
  , ("A"        , unitBase) -- ampere
  , ("cd"       , unitBase) -- candela

  , ("rad"       , unitBase) -- angles
  , ("deg"       , unitBase) -- angles
 ]

-- | The type constructors in scope (and their kinds), which the kind and
-- type checker read via the implicit parameter ?typeConstructors. This starts
-- out as 'primitiveTypeConstructors' and is to be extended by user
-- declarations of new constructors of open kinds (e.g. new units of measure).
type TypeConstructors = Map.Map Identifier (Type 1)

type HasTypeConstructors = (?typeConstructors :: TypeConstructors)

-- | The built-in type constructors. On duplicate names the first entry in
-- 'typeConstructors' wins, matching the previous 'lookup' behaviour.
primitiveTypeConstructors :: TypeConstructors
primitiveTypeConstructors = Map.fromListWith (\_new old -> old) typeConstructors

typeAliases :: [(Identifier, Type 0)]
typeAliases = [
    ("str", stringTy (tyCon0 "1"))
  , ("int", integerTy (tyCon0 "1"))
  , ("float", floatTy (tyCon0 "1"))
  , ("bool", boolTy (tyCon0 "1"))
 ]

numericalTypes :: [Identifier]
numericalTypes = ["Float", "Integer"]

kindConstructors :: [(Identifier, Type 2)]
kindConstructors = [
    ("UoM"      , desc2)
  , ("KoQ"      , desc2)
  , ("SpeciesType", desc2)
  , ("BasisType" , desc2)
  , ("Dim"      , desc2)
  , ("AffineSpace", desc2)
  -- Products of descriptors
  , ("Base"     , desc2) -- The base Descriptor (bottom)
  , ("&"        , FunTy [desc2, desc2] desc2)
  ]


base :: Type 1
base = tyCon1 "Base"

agroup :: Type 1
agroup = tyCon1 "AGroup"

-- | Closed domain kind for Dimension: only the standard 7 SI base
-- dimensions (and their products/powers) have this kind, unlike Unit's
-- argument (any Type) which is an open set of generators.
dimensionBase :: Type 1
dimensionBase = tyCon1 "DimensionBase"

-- This is actually an open type
unitBase :: Type 1
unitBase = tyCon1 "UnitBase"

-- | Open kinds: those to which programs may add new constructors with a
-- class declaration, e.g. `class _(UnitBase): km : UnitBase`. All other kinds
-- are closed (e.g. DimensionBase, whose constructors are fixed by the SI
-- coherence map in 'rewriteSI').
openKinds :: [Type 1]
openKinds = [unitBase]

-- TODO: later this will need to be something that is parameterised
-- and threaded through when we start making things more open
-- | Check if a type constructors a descriptor
isDescConstructor :: Identifier -> Maybe (Type 1)
isDescConstructor conId =
  case lookup conId typeConstructors of
    Just k@(FunTy [t] _) | t == desc -> Just k
    Just k@(ImplicitFunTy _ t _) | t == desc2 -> Just k
    _                              -> Nothing

