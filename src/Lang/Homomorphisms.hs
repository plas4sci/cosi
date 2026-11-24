{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Lang.Homomorphisms where

import Lang.Syntax

--------------------
-- # General helpers

-- | Lift a map on constructor names to a homomorphism on descriptors (partial field)
-- mapping a description: generators are renamed, and the remaining structure is preserved
-- Assumes we have a 'partial field' expression here (i.e., that some kind of normalisation
-- has already been applied)
constructorMapToHomom :: (String -> Maybe String) -> Type 0 -> Maybe (Type 0)
constructorMapToHomom f = go
  where
    go :: Type 0 -> Maybe (Type 0)
    go t@(TyCon ZeroP c)
      | Just _ <- rationalOfType t = return t   -- numerals are fixed
      -- if the mapping is undefined then the whole thing is undefined
      | otherwise = TyCon ZeroP <$> f c
    go (SumTy t1 t2)    = SumTy <$> go t1 <*> go t2
    go (ProdTy t1 t2)   = ProdTy <$> go t1 <*> go t2
    go (ExponentTy t q) = (`ExponentTy` q) <$> go t
    go (NegTy t)        = NegTy <$> go t
    -- rational literals and type variables are not generators
    go t@(TyRat _)      = return t
    go t@(TyVar _)      = return t
    -- anything else is not part of a field's group structure
    go t                = return t

-----------------------
-- # Specific instances

rewriteSI :: String -> Maybe String
rewriteSI "T" = Just "s"
rewriteSI "L" = Just "m"
rewriteSI "M" = Just "kg"
rewriteSI "Theta" = Just "K"
rewriteSI "N" = Just "mol"
rewriteSI "U" = Just "A"
rewriteSI "Ji" = Just "cd"
rewriteSI x = Nothing

-- | The dimension of an SI base unit, exactly the inverse of rewriteSI (so
-- unlike 'dimensionOfSIUnit', a scaled unit such as km has no image)
dimensionOfSIBaseUnit :: String -> Maybe String
dimensionOfSIBaseUnit u = lookup u
  [ (unit, dim) | dim <- ["T", "L", "M", "Theta", "N", "U", "Ji"], Just unit <- [rewriteSI dim] ]

-- | The base dimension of a unit, coherent with rewriteSI
dimensionOfSIUnit :: String -> Maybe String
dimensionOfSIUnit u = lookup u
  [("s", "T"), ("m", "L"), ("km", "L"), ("kg", "M"), ("K", "Theta"), ("mol", "N"), ("A", "U"), ("cd", "Ji")]

-- | The partial homomorphism from the affine-space descriptor to dimensions:
-- a scalar Scalar[c] is a pure number, so it is dimensionless, whereas a
-- Point, Vector or Affine[w] can have any dimension, so it has no image.
-- Products, powers and negations of scalars are scalars, and are sent to 1;
-- anything involving a non-scalar is undefined, so that the domain reflects
-- definedness (f(a * b) is defined iff f(a) and f(b) are)
affineSpaceToDimension :: Type 0 -> Maybe (Type 0)
affineSpaceToDimension t
  | isScalar t = Just (TyCon ZeroP "1")
  | otherwise  = Nothing
  where
    isScalar :: Type 0 -> Bool
    isScalar (TyApp (TyCon ZeroP "Scalar") [_]) = True
    isScalar (ProdTy t1 t2)   = isScalar t1 && isScalar t2
    isScalar (ExponentTy t' _) = isScalar t'
    isScalar (NegTy t')        = isScalar t'
    isScalar _                 = False

-- | This data structure holds homomorphisms between different descriptors
-- and flags whether they are coherence maps or not
homomorphisms :: [((Identifier, Identifier), (Type 0 -> Maybe (Type 0), Bool))]
homomorphisms = [
   (("UoM", "Dim"), (constructorMapToHomom dimensionOfSIUnit, True))
 , (("Dim", "UoM"), (constructorMapToHomom rewriteSI, False))
 , (("AffineSpace", "Dim"), (affineSpaceToDimension, True))

 ]
