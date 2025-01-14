{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Numeric.Mesh.Gmsh.Model
  ( addPhysicalGroup',
    addPhysicalGroup,
    setPhysicalName,
    addPhysicalVolume,
    addPhysicalSurface,
    addPhysicalCurve,
    addPhysicalPoint,
    getEntities,
    getAllEntities,
    getVolumes,
    getSurfaces,
    getCurves,
    getPoints,
    getBoundary,
    getBoundary',
    BoundaryOpts (..),
    getAdjacencies,
    Adjacencies (..),
    getWholeBoundingBox,
    getBoundingBox,
    getEntitiesInBoundingBox,
    getPointsInBoundingBox,
    getCurvesInBoundingBox,
    getSurfacesInBoundingBox,
    getVolumesInBoundingBox,
    getAllEntitiesInBoundingBox,
    remove',
    remove,
  )
where

import qualified Control.Foldl as L
import Control.Monad.IO.Class (MonadIO)
import Data.Coerce (coerce)
import qualified Data.Foldable as F
import Data.Proxy (Proxy (Proxy))
import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified Numeric.Mesh.Gmsh.LowLevel as Low
import Numeric.Mesh.Gmsh.Types
import Type.Reflection (Typeable)

addPhysicalGroup' ::
  forall ent t m.
  (HasDimension ent, Foldable t, MonadIO m) =>
  t (Entity ent) ->
  Maybe (PhysicalTag ent) ->
  GmshT m (PhysicalGroup ent)
addPhysicalGroup' ents mtag = do
  fmap PhysicalGroup $
    embedLow $
      Low.gmshModelAddPhysicalGroup
        (entityDim @ent Proxy)
        (L.fold (L.premap coerce L.vector) ents)
        (maybe (-1) coerce mtag)

addPhysicalGroup ::
  forall ent t m.
  (HasDimension ent, Foldable t, MonadIO m) =>
  PhysicalName ->
  t (Entity ent) ->
  GmshT m (PhysicalGroup ent)
addPhysicalGroup name ents = do
  pg <- addPhysicalGroup' ents Nothing
  setPhysicalName pg name
  pure pg

addPhysicalVolume ::
  (Foldable t, MonadIO m) =>
  PhysicalName ->
  t Volume ->
  GmshT m PhysicalVolume
addPhysicalVolume = addPhysicalGroup @VolumeE

addPhysicalSurface ::
  (Foldable t, MonadIO m) =>
  PhysicalName ->
  t Surface ->
  GmshT m PhysicalSurface
addPhysicalSurface = addPhysicalGroup @SurfaceE

addPhysicalCurve ::
  (Foldable t, MonadIO m) =>
  PhysicalName ->
  t Curve ->
  GmshT m PhysicalCurve
addPhysicalCurve = addPhysicalGroup @CurveE

addPhysicalPoint ::
  (Foldable t, MonadIO m) =>
  PhysicalName ->
  t Point ->
  GmshT m PhysicalPoint
addPhysicalPoint = addPhysicalGroup @PointE

setPhysicalName ::
  forall ent m.
  (HasDimension ent, MonadIO m) =>
  PhysicalGroup ent ->
  PhysicalName ->
  GmshT m ()
setPhysicalName physs name =
  embedLow_ $
    Low.gmshModelSetPhysicalName (toRawPhysicalGroup physs) name

getEntities ::
  forall ent m.
  (HasDimension ent, MonadIO m) =>
  GmshT m (V.Vector (Entity ent))
getEntities = do
  dimTags <- embedLow $ Low.gmshModelGetEntities $ entityDim @ent Proxy
  pure $ V.map (Entity . Low.entityTag) $ V.convert dimTags

getVolumes :: MonadIO m => GmshT m (V.Vector Volume)
getVolumes = getEntities @VolumeE

getSurfaces :: MonadIO m => GmshT m (V.Vector Surface)
getSurfaces = getEntities

getCurves :: MonadIO m => GmshT m (V.Vector Curve)
getCurves = getEntities

getPoints :: MonadIO m => GmshT m (V.Vector Point)
getPoints = getEntities

getAllEntities ::
  (MonadIO m) =>
  GmshT m (V.Vector SomeEntity)
getAllEntities = do
  dimTags <- embedLow $ Low.gmshModelGetEntities (-1)
  pure $ V.map toSomeEntity $ V.convert dimTags

data BoundaryOpts = BoundaryOpts
  { -- | If 'True', returns the boundary of the combined geometrical shape formed by all input entities;
    -- otherwise returns the boundary of the individual entities.
    bdCombined :: Bool
  , -- | Multiplies the ID by the sign if 'True'.
    bdOriented :: Bool
  , -- | Returns transitive boundary recursively or not.
    bdRecursive :: Bool
  }
  deriving (Read, Show, Eq, Ord, Typeable, Generic)

getBoundary' ::
  (MonadIO m, Foldable t) =>
  t SomeEntity ->
  BoundaryOpts ->
  GmshT m (V.Vector SomeEntity)
getBoundary' entities BoundaryOpts {..} =
  fmap (V.map toSomeEntity . V.convert) $
    embedLow $
      Low.gmshModelGetBoundary
        (L.fold (L.premap someToRawEntity L.vector) entities)
        bdCombined
        bdOriented
        bdRecursive

getBoundary ::
  ( HasDimension e
  , Foldable t
  , MonadIO m
  , KnownEntity (Boundary e)
  ) =>
  t (Entity e) ->
  BoundaryOpts ->
  GmshT m (V.Vector (Entity (Boundary e)))
getBoundary entities BoundaryOpts {..} =
  fmap (V.mapMaybe fromRawEntity . V.convert) $
    embedLow $
      Low.gmshModelGetBoundary
        (L.fold (L.premap toRawEntity L.vector) entities)
        bdCombined
        bdOriented
        bdRecursive

data Adjacencies e = Adjacencies
  { upward :: V.Vector (Entity (Extruded e))
  , downward :: V.Vector (Entity (Boundary e))
  }
  deriving (Eq, Ord, Generic)

deriving instance
  ( KnownEntity e
  , KnownEntity (Extruded e)
  , KnownEntity (Boundary e)
  ) =>
  Show (Adjacencies e)

getAdjacencies ::
  ( MonadIO m
  , HasDimension e
  , HasDimension (Extruded e)
  , HasDimension (Boundary e)
  ) =>
  Entity e ->
  -- | Upwards and Downwards
  GmshT m (Adjacencies e)
getAdjacencies =
  fmap
    ( \Low.Adjacency {..} ->
        Adjacencies
          { upward = V.map Entity $ V.convert upward
          , downward = V.map Entity $ V.convert downward
          }
    )
    . embedLow
    . Low.gmshModelGetAdjacencies
    . toRawEntity

-- | Get the bounding box @(minCoord, maxCoord)@ of the whole model.
getWholeBoundingBox :: MonadIO m => GmshT m (Coord, Coord)
{-# INLINE getWholeBoundingBox #-}
getWholeBoundingBox =
  embedLow $ Low.gmshModelGetBoundingBox (Low.Entity (-1) (-1))

-- | Get the bounding box @(minCoord, maxCoord)@ of the model entity.
getBoundingBox ::
  (HasDimension e, MonadIO m) =>
  Entity e ->
  GmshT m (Coord, Coord)
{-# INLINE getBoundingBox #-}
getBoundingBox = embedLow . Low.gmshModelGetBoundingBox . toRawEntity

{- |
Get the model entities in the bounding box defined by the two points
@minCoord@ and @maxCoord@.
-}
getEntitiesInBoundingBox ::
  forall e m.
  (HasDimension e, MonadIO m) =>
  -- | @minCoord@
  Coord ->
  -- | @maxCoord@
  Coord ->
  GmshT m (V.Vector (Entity e))
getEntitiesInBoundingBox minCoord maxCoord =
  fmap (V.mapMaybe fromRawEntity . V.convert) $
    embedLow $
      Low.gmshModelGetEntitiesInBoundingBox minCoord maxCoord $
        fromIntegral $ entityDim $ Proxy @e

-- | Get the model points in the bounding box defined by the two points @minCoord@ and @maxCoord@.
getPointsInBoundingBox :: MonadIO m => Coord -> Coord -> GmshT m (V.Vector Point)
getPointsInBoundingBox = getEntitiesInBoundingBox @PointE

-- | Get the model curves in the bounding box defined by the two points @minCoord@ and @maxCoord@.
getCurvesInBoundingBox :: MonadIO m => Coord -> Coord -> GmshT m (V.Vector Curve)
getCurvesInBoundingBox = getEntitiesInBoundingBox

-- | Get the model surfaces in the bounding box defined by the two points @minCoord@ and @maxCoord@.
getSurfacesInBoundingBox :: MonadIO m => Coord -> Coord -> GmshT m (V.Vector Surface)
getSurfacesInBoundingBox = getEntitiesInBoundingBox

-- | Get the model volumes in the bounding box defined by the two points @minCoord@ and @maxCoord@.
getVolumesInBoundingBox :: MonadIO m => Coord -> Coord -> GmshT m (V.Vector Volume)
getVolumesInBoundingBox = getEntitiesInBoundingBox

{- |
Get all the model entities in the bounding box defined by the two points
@minCoord@ and @maxCoord@.
-}
getAllEntitiesInBoundingBox ::
  (MonadIO m) =>
  -- | @minCoord@
  Coord ->
  -- | @maxCoord@
  Coord ->
  GmshT m (V.Vector SomeEntity)
getAllEntitiesInBoundingBox minCoord maxCoord =
  fmap (V.map toSomeEntity . V.convert) $
    embedLow $
      Low.gmshModelGetEntitiesInBoundingBox minCoord maxCoord (-1)

remove' :: (Foldable t, MonadIO m) => Bool -> t SomeEntity -> GmshT m ()
remove' recursive ent =
  embedLow_ $
    Low.gmshModelOccRemove (L.fold (L.premap someToRawEntity L.vector) ent) recursive

remove ::
  (HasDimension e, Foldable t, MonadIO m) =>
  Bool ->
  t (Entity e) ->
  GmshT m ()
remove recursive = remove' recursive . map MkSomeEntity . F.toList
