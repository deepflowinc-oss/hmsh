{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.Mesh.Gmsh.Helper where

import Barbies
import Control.Monad.Trans.Accum (add, evalAccum, look, runAccum)
import Data.Functor.Compose
import Data.Maybe (fromMaybe)
import Data.Monoid (Ap (Ap, getAp), Sum (Sum))
import Data.Proxy (Proxy (Proxy))
import Data.Semigroup.Generic
import Data.Type.Equality (testEquality, type (:~:) (Refl), type (~~))
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Numeric.Mesh.Gmsh (OptionArg (BoolArg), withOption)
import Numeric.Mesh.Gmsh.Helper.Parser (someExtrusionP)
import Numeric.Mesh.Gmsh.Types
import Numeric.Mesh.Gmsh.Utils
import qualified Text.Regex.Applicative as RE
import UnliftIO (MonadUnliftIO)
import qualified VectorBuilder.Builder as VB
import qualified VectorBuilder.Vector as VB

data EntityIndices h = EntityIndices
  { pointIndices :: h Int
  , curveIndices :: h Int
  , surfaceIndices :: h Int
  , volumeIndices :: h Int
  }
  deriving (Generic)
  deriving anyclass (FunctorB, TraversableB, ConstraintsB)

deriving via
  GenericSemigroupMonoid (EntityIndices h)
  instance
    (Semigroup (h Int)) => Semigroup (EntityIndices h)

deriving via
  GenericSemigroupMonoid (EntityIndices h)
  instance
    (Monoid (h Int)) => Monoid (EntityIndices h)

deriving instance (Eq (h Int)) => Eq (EntityIndices h)

deriving instance (Ord (h Int)) => Ord (EntityIndices h)

deriving instance (Show (h Int)) => Show (EntityIndices h)

withExtruderNoLateral ::
  ( MonadUnliftIO m
  , Traversable t
  , TraversableB h
  , ConstraintsB h
  , AllB Extrusible h
  ) =>
  ( V.Vector SomeEntity ->
    Vec3 ->
    [Int] ->
    [Double] ->
    Bool ->
    GmshT m (V.Vector SomeEntity)
  ) ->
  h t ->
  Vec3 ->
  [Int] ->
  [Double] ->
  GmshT m (h (Compose t Extrusion))
withExtruderNoLateral = withExtruder NoLateral

withExtruderWithLateral ::
  ( MonadUnliftIO m
  , Traversable t
  , TraversableB h
  , ConstraintsB h
  , AllB Extrusible h
  ) =>
  ( V.Vector SomeEntity ->
    Vec3 ->
    [Int] ->
    [Double] ->
    Bool ->
    GmshT m (V.Vector SomeEntity)
  ) ->
  h t ->
  Vec3 ->
  [Int] ->
  [Double] ->
  GmshT m (h (Compose t Extrusion))
withExtruderWithLateral = withExtruder WithLateral

withExtruder ::
  ( MonadUnliftIO m
  , Traversable t
  , TraversableB h
  , ConstraintsB h
  , AllB Extrusible h
  ) =>
  ExtrusionMode ->
  ( V.Vector SomeEntity ->
    Vec3 ->
    [Int] ->
    [Double] ->
    Bool ->
    GmshT m (V.Vector SomeEntity)
  ) ->
  h t ->
  Vec3 ->
  [Int] ->
  [Double] ->
  GmshT m (h (Compose t Extrusion))
withExtruder eMode extruder (entities :: h t) dir numEntities heights =
  withOption "Geometry.ExtrudeReturnLateralEntities" (BoolArg $ eMode == WithLateral) $ do
    let total :: Int
        assigns0 :: EntityIndices VB.Builder
        ((ents0, assigns0), Sum total) =
          flip runAccum 0 $
            getAp $
              bfoldMapC @Extrusible @h
                ( foldMap $ \(e :: Entity e) -> Ap $ do
                    Sum i <- look
                    add $ Sum 1
                    let idx = case sEntityVal' @e of
                          SPointE -> mempty {pointIndices = VB.singleton i}
                          SCurveE -> mempty {curveIndices = VB.singleton i}
                          SSurfaceE -> mempty {surfaceIndices = VB.singleton i}
                          _ -> mempty
                    pure (VB.singleton $ MkSomeEntity e, idx)
                )
                entities
        ents = VB.build ents0
        flattenAssigns = bfoldMapC @(Equal Int) (VB.build @V.Vector) assigns0
    ents' <- extruder ents dir numEntities heights True
    let somePat =
          sequenceA $
            V.generate
              (VB.size $ pointIndices assigns0)
              (const $ someExtrusionP eMode PointE)
              <> V.generate
                (VB.size $ curveIndices assigns0)
                (const $ someExtrusionP eMode CurveE)
              <> V.generate
                (VB.size $ surfaceIndices assigns0)
                (const $ someExtrusionP eMode SurfaceE)
        eGroups :: V.Vector (ExistsC Extrusible Extrusion)
        eGroups =
          fromMaybe
            (error "Invalid input!")
            $ RE.match somePat $ V.toList ents'
        ents'' = V.update_ (V.replicate total undefined) flattenAssigns eGroups
    pure $
      flip evalAccum (Sum (0 :: Int)) $
        btraverseC @Extrusible
          ( \(es :: t (Entity e)) ->
              Compose
                <$> traverse
                  ( \_ -> do
                      Sum i <- look
                      add 1
                      case ents'' V.! i of
                        SomeC (e :: Extrusion (Entity e'))
                          | Just Refl <-
                              testEquality (sEntityVal' @e) (sEntityVal' @e') ->
                            pure e
                          | otherwise -> error $ "EntityType mismatched: " <> show (entityVal @e Proxy, entityVal @e' Proxy)
                  )
                  es
          )
          entities

class b ~~ a => Equal a b

instance b ~~ a => Equal a b
