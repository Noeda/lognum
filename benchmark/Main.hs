{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE BangPatterns #-}

module Main ( main ) where

import Control.Exception
import Criterion
import Criterion.Main
import Numeric.LogBase
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = defaultMain $
  [env (evaluate $ V.replicate 1000 0.3) $ \p ->
     bgroup "simple"
       [ bench "simpleCompute" $ whnf compute p ]
       , let struct = Struct 0.5 1.0
          in bench "simpleComputeStructUnpacked" $ whnf computeStruct struct
       , let struct = StructP 0.5 1.0
          in bench "simpleComputeStructPoly" $ whnf computeStructPoly struct ]

data Struct = Struct
  { horsePower :: {-# UNPACK #-} !(LB Double)
  , horseHome :: {-# UNPACK #-} !(LB Double) }

data StructP a = StructP
  { horsePowerP :: !a
  , horseHomeP :: !a }
  deriving ( Functor, Foldable, Traversable )

compute :: V.Vector (LB Double) -> V.Vector (LB Double)
compute = V.map $ \v -> v + v*v
{-# NOINLINE compute #-}

computeStruct :: Struct -> Struct
computeStruct struct =
  struct { horsePower = op $ horsePower struct
         , horseHome = op $ horseHome struct }
 where
  op x = x + x*x
  {-# INLINE op #-}
{-# NOINLINE computeStruct #-}

computeStructPoly :: StructP (LB Double) -> StructP (LB Double)
computeStructPoly = fmap op
 where
  op x = x + x*x
  {-# INLINE op #-}
{-# NOINLINE computeStructPoly #-}

