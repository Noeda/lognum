{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Numeric.LogBase
  ( LB()
  , LBD
  , LBF
  , liftLogBase
  , unwrapLogBase )
  where

import Data.Binary
import Data.Data
import Data.Hashable
import Data.Monoid
import Data.Ratio
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

-- | Convience type synonym.
type LBD = LB Double
-- | Convience type synonym.
type LBF = LB Float

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

instance (Floating a, Show a) => Show (LB a) where
  show (LB val) = "<LB " <> show val <> " --> " <> show (exp val) <> ">"

instance (Ord a, Floating a, RealFloat a) => Num (LB a) where
  LB num1 * LB num2 = LB $ num1 + num2
  {-# INLINE (*) #-}

  LB num1 + LB num2 | num1 >= num2 =
    LB $ num1 + log (1 + exp (num2 - num1))
  LB num1 + LB num2 = LB num2 + LB num1
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

