{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Numeric.LogBase
  ( LB()
  , LBD
  , LBF
  , toLogBase
  , liftLogBase
  , unwrapLogBase
  , wrapLogBase )
  where

import Data.Binary
import Data.Data
import Data.Hashable
import Data.Monoid
import Data.Ratio
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as V
import GHC.Generics

-- | An `LB` is a numerical value. `LB` that internally keeps its value as @log
-- x@ instead of just @x@, and implements the most common Haskell numerical
-- typeclasses on it so it can be used in many contexts.
--
-- `LB` can represent extremely small and large values that ordinary numerical
-- types cannot. In particular, it's good for multiplying very large number of
-- small (<0.1) values together (often arises when computing small
-- probabilities) and not losing on precision too much.
--
-- Multiplication, division and power (`**`) work well on `LB`s. Addition works
-- well as long as the operands are not very far apart.
--
-- `LB` cannot hold negative values. Some operations check for this but others
-- may result in NaN.
newtype LB a = LB a
  deriving ( Eq, Ord, Typeable, Data, Generic, Binary, Hashable )

newtype instance V.MVector s (LB a) = MV_LB (V.MVector s a)
newtype instance V.Vector (LB a) = V_LB (V.Vector a)

instance V.Unbox a => VGM.MVector V.MVector (LB a) where
  basicLength (MV_LB vec) = VGM.basicLength vec
  {-# INLINE basicLength #-}

  basicOverlaps (MV_LB vec1) (MV_LB vec2) = VGM.basicOverlaps vec1 vec2
  {-# INLINE basicOverlaps #-}

  basicUnsafeNew x = MV_LB <$> VGM.basicUnsafeNew x
  {-# INLINE basicUnsafeNew #-}

  basicInitialize (MV_LB vec) = VGM.basicInitialize vec
  {-# INLINE basicInitialize #-}

  basicUnsafeSlice start offset (MV_LB vec) =
    MV_LB $ VGM.basicUnsafeSlice start offset vec
  {-# INLINE basicUnsafeSlice #-}

  basicUnsafeRead (MV_LB vec) idx = LB <$> VGM.basicUnsafeRead vec idx
  {-# INLINE basicUnsafeRead #-}

  basicUnsafeWrite (MV_LB vec) idx (LB val) = VGM.basicUnsafeWrite vec idx val
  {-# INLINE basicUnsafeWrite #-}

  basicSet (MV_LB vec) (LB val) = VGM.basicSet vec val
  {-# INLINE basicSet #-}

instance V.Unbox a => VG.Vector V.Vector (LB a) where
  basicLength (V_LB vec) = VG.basicLength vec
  {-# INLINE basicLength #-}

  basicUnsafeFreeze (MV_LB vec) = V_LB <$> VG.basicUnsafeFreeze vec
  {-# INLINE basicUnsafeFreeze #-}

  basicUnsafeThaw (V_LB vec) = MV_LB <$> VG.basicUnsafeThaw vec
  {-# INLINE basicUnsafeThaw #-}

  basicUnsafeSlice start offset (V_LB vec) =
    V_LB $ VG.basicUnsafeSlice start offset vec
  {-# INLINE basicUnsafeSlice #-}

  basicUnsafeIndexM (V_LB vec) idx =
    LB <$> VG.basicUnsafeIndexM vec idx
  {-# INLINE basicUnsafeIndexM #-}

  basicUnsafeCopy (MV_LB vec) (V_LB vec2) = VG.basicUnsafeCopy vec vec2
  {-# INLINE basicUnsafeCopy #-}

  elemseq (V_LB vec) (LB a) b = VG.elemseq vec a b
  {-# INLINE elemseq #-}

instance V.Unbox a => V.Unbox (LB a)

-- | Convience type synonym.
type LBD = LB Double
-- | Convience type synonym.
type LBF = LB Float

-- | Converts an ordinary number to `LB` number.
toLogBase :: Floating a => a -> LB a
toLogBase value = LB $ log value
{-# INLINE toLogBase #-}

-- | Converts `LB` number to its base type.
--
-- The base may not be able to represent the `LB` value correctly though.
liftLogBase :: Floating a => LB a -> a
liftLogBase (LB num) = exp num
{-# INLINE liftLogBase #-}

-- | Unwraps `LB` number to its base type, without any conversion.
--
-- This returns the actual number used internally inside `LB`.
unwrapLogBase :: LB a -> a
unwrapLogBase (LB num) = num
{-# INLINE unwrapLogBase #-}

-- | Wraps a number to `LB`, without any conversion.
wrapLogBase :: a -> LB a
wrapLogBase num = LB num
{-# INLINE wrapLogBase #-}

instance (Floating a, Show a) => Show (LB a) where
  show (LB val) = "<LB " <> show val <> " --> " <> show (exp val) <> ">"

instance (Ord a, Floating a, RealFloat a) => Num (LB a) where
  LB num1 * LB num2 = LB $ num1 + num2
  {-# INLINE (*) #-}

  LB num1 + LB num2 =
    if num1 > num2
      then LB $ num1 + log (1 + exp (num2 - num1))
      else LB $ num2 + log (1 + exp (num1 - num2))
  {-# INLINE (+) #-}

  LB num1 - LB num2 | num1 >= num2 =
    LB $ num1 + log (1 - exp (num2 - num1))
  LB{} - LB {} = error "Subtraction would result in negative value."
  {-# INLINE (-) #-}

  fromInteger int
    | int < 0 = error "Logbase numbers cannot be negative."
    | otherwise = LB $ log $ fromIntegral int
  {-# INLINE fromInteger #-}

  abs val = val
  {-# INLINE abs #-}

  signum (LB val)
    | isInfinite val = 0
    | otherwise = 1
  {-# INLINE signum #-}

instance RealFloat a => Fractional (LB a) where
  LB num1 / LB num2 = LB (num1 - num2)
  {-# INLINE (/) #-}

  recip num = 1 / num
  {-# INLINE recip #-}

  fromRational ratio = fromInteger (numerator ratio) / fromInteger (denominator ratio)
  {-# INLINE fromRational #-}

-- | Note that `log` will result in NaN if the actual value is less than 1.
--
-- This typeclass is implemented for completeness but almost all operations
-- (aside from `log` and `exp` and `**`) are implemented by lifting up from
-- logbase, then doing the operation and then going back to logbase. Which is
-- why they are unlikely to work for very small values. Given that you probably
-- are working on logbase in the first place BECAUSE of being able to represent
-- small values, relying on functions here is possibly not a good idea.
instance RealFloat a => Floating (LB a) where
  pi = LB $ log pi
  {-# INLINE pi #-}

  LB num1 ** LB num2 = LB $ num1 * exp num2
  {-# INLINE (**) #-}

  exp (LB num1) = LB $ exp num1
  {-# INLINE exp #-}

  log (LB num1) = LB $ log num1
  {-# INLINE log #-}

  sqrt (LB num1) = LB $ log $ sqrt $ exp num1
  {-# INLINE sqrt #-}

  sin (LB num1) = LB $ log $ sin $ exp num1
  cos (LB num1) = LB $ log $ cos $ exp num1
  asin (LB num1) = LB $ log $ asin $ exp num1
  acos (LB num1) = LB $ log $ acos $ exp num1
  atan (LB num1) = LB $ log $ atan $ exp num1
  sinh (LB num1) = LB $ log $ sinh $ exp num1
  cosh (LB num1) = LB $ log $ cosh $ exp num1
  asinh (LB num1) = LB $ log $ asinh $ exp num1
  acosh (LB num1) = LB $ log $ acosh $ exp num1
  atanh (LB num1) = LB $ log $ atanh $ exp num1

-- | Lifts from logbase.
instance (Real a, Floating a, RealFloat a) => Real (LB a) where
  toRational (LB num1) = toRational $ exp num1
  {-# INLINE toRational #-}

-- | Lifts from logbase.
instance (RealFloat a, Floating a, RealFrac a) => RealFrac (LB a) where
  properFraction (LB num1) =
    let (int, fractional) = properFraction $ exp num1
     in (int, LB $ log fractional)
  {-# INLINE properFraction #-}

