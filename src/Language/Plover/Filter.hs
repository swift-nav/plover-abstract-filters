{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.Plover.Filter where

import Language.Plover.Quote
import Language.Plover.ParserTypes
import Numeric.AD

data State a = State
  { x :: a
  , v :: a
  , n :: a
  } deriving (Show, Eq, Traversable, Foldable, Functor)

data Observation a = Observation
  { code :: a
  , carrier :: a
  } deriving (Show, Eq, Traversable, Foldable, Functor)

obsModel :: (Num a) => State a -> Observation a
obsModel State{..} = Observation
                     { code = x
                     , carrier = x + n
                     }

dynamicsModel :: (Num a) => a -> State a -> State a
dynamicsModel dt State{..} = State
                             { x = x^2 + v * dt
                             , v = v
                             , n = n
                             }

flattenVec v = foldr (:) [] v
flattenMtx m = flattenVec $ fmap flattenVec m

--flatJacobian f x = flattenMtx $ jacobian f x


-- pseudoplover:
--
-- predict (x :: double[n], dt :: double) :: double[n]
--   flattenVec State {..names..} <- x
--   F <- dynamicsModel dt {..names..}
--   x' <- F x
--   return x'
--
-- predObs (x :: double[n]) :: double[m]
--   flattenVec State {..names..} <- x
--   H <- obsModel dt {..names..}
--   z_tilde <- H x
--   return z_tilde

xx :: State Expr
xx = State [pexp| x :: Double |]
           [pexp| v :: Double |]
           [pexp| n :: Double |]


rot_small :: Expr -> Expr
rot_small x = [pexp| vec(vec(1.0, ~x, 0.0),
                         vec(- ~x, 1.0, 0.0),
                         vec(0.0, 0.0, 1.0)) |]

cunit = [ptop|
         foo (v :: double[3]) :: double[3]
           := ~(rot_small 22.0) * v;
        |]

f :: Expr -> Expr
f [pexp| ~x + ~y |] = x * y
f z = z

--main = compileUnit "pvt" [f, cunit

