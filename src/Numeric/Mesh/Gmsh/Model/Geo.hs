{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Numeric.Mesh.Gmsh.Model.Geo
  ( addPoint,
    addPointAs,
    addPoint',
    PointConfig (..),
    addLine,
    addLine',
    synchronize,
    addCircleArc,
    addCircleArcAs,
    CircleArcConfig (..),
    addCircleArc',
    addCircleArcOn,
    EllipseArcConfig (..),
    addEllipseArc',
    addEllipseArc,
    addEllipseArcAs,
    addEllipseArcOn,
    addBezier,
    addBezierAs,
    addSpline,
    addSplineAs,
    CurveLoopConfig (..),
    addCurveLoop,
    addCurveLoop',
    addPlaneSurface,
    addPlaneSurfaceAs,
    extrude,
    extrude',
    copy,
    copy',
    extrudeWithLateralsB,
    extrudeNoLateralB,
    revolve,
    revolve',
    mirror,
    mirror',
    removeAllDuplicates,
  )
where

import Barbies (AllB, ConstraintsB, TraversableB)
import qualified Control.Foldl as L
import Control.Monad.IO.Class (MonadIO)
import Data.Coerce (coerce)
import qualified Data.Foldable as F
import Data.Functor.Compose (Compose)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import GHC.Generics (Generic)
import Numeric.Mesh.Gmsh.Helper (withExtruderNoLateral, withExtruderWithLateral)
import Numeric.Mesh.Gmsh.LowLevel (gmshModelGeoAddBezier, gmshModelGeoAddEllipseArc, gmshModelGeoAddSpline)
import qualified Numeric.Mesh.Gmsh.LowLevel as Low
import Numeric.Mesh.Gmsh.Types
import Type.Reflection (Typeable)
import UnliftIO (MonadUnliftIO)

{- |
Add a geometrical point in the built-in CAD representation, at the given coordinate.
-}
addPoint :: MonadIO m => Coord -> GmshT m Point
addPoint cd = fmap Entity $ embedLow $ Low.gmshModelGeoAddPoint cd 0.0 (-1)

{- |
Add a geometrical point in the built-in CAD representation, at the given coordinate with the given @tag@.
-}
addPointAs :: MonadIO m => Coord -> PointTag -> GmshT m Point
addPointAs cd (EntityTag tag) =
  fmap Entity $ embedLow $ Low.gmshModelGeoAddPoint cd 0.0 tag

{- |
Add a geometrical point in the built-in CAD representation and returns the tag of the new point.

Note that the point will be added in the current model only after @synchronize@ is called. This
behavior holds for all the entities added in the geo module.
-}
addPoint' :: MonadIO m => PointConfig -> GmshT m Point
addPoint' Point {..} =
  fmap Entity $
    embedLow $
      Low.gmshModelGeoAddPoint
        pointCoord
        (maybe 0.0 realToFrac pointMeshSize)
        (maybe (-1) getRawEntityTag pointNewTag)

{- |
Add a straight line segment in the built-in CAD representation, between the
two points with tags @start@ and @end@. If @tag@ is positive, set the
tag explicitly; otherwise a new tag is selected automatically. Return the
tag of the line.
-}
addLine' :: MonadIO m => Point -> Point -> CurveTag -> GmshT m Curve
addLine' start end tag =
  fmap Entity $
    embedLow $
      Low.gmshModelGeoAddLine (coerce start) (coerce end) (coerce tag)

{- |
Add a straight line segment in the built-in CAD representation, between the
two points with tags @start@ and @end@.
-}
addLine :: MonadIO m => Point -> Point -> GmshT m Curve
addLine start end = addLine' start end (-1)

{- |
Synchronize the built-in CAD representation with the current Gmsh model.
This can be called at any time, but since it involves a non trivial amount
of processing, the number of synchronization points should normally be
minimized. Without synchronization the entities in the built-in CAD
representation are not available to any function outside of the built-in
CAD kernel functions.
-}
synchronize :: MonadIO m => GmshT m ()
synchronize = embedLow_ Low.gmshModelGeoSynchronize

{- |
Add a circle arc (strictly smaller than 'pi') in the built-in CAD
representation, between the two points with tags @start@ and @end@,
and with center @center@.
-}
addCircleArc ::
  MonadIO m =>
  -- | start
  Point ->
  -- |  center
  Point ->
  -- |  end
  Point ->
  GmshT m Curve
addCircleArc (Entity begin) (Entity center) (Entity end) =
  fmap (Entity @CurveE) $
    embedLow $
      Low.gmshModelGeoAddCircleArc begin center end (-1) (Vec3 0 0 0)

{- |
Add a circle arc (strictly smaller than 'pi') in the built-in CAD
representation, between the two points with tags @start@ and @end@,
and with center @center@, placed on the plane defined by @normal@.
-}
addCircleArcOn ::
  MonadIO m =>
  -- | start
  Point ->
  -- | center
  Point ->
  -- | end
  Point ->
  -- | normal
  Vec3 ->
  GmshT m Curve
addCircleArcOn (Entity begin) (Entity center) (Entity end) normal =
  fmap (Entity @CurveE) $
    embedLow $
      Low.gmshModelGeoAddCircleArc begin center end (-1) normal

{- |
Add a circle arc (strictly smaller than 'pi') in the built-in CAD
representation, between the two points with tags @start@ and @end@,
and with center @center@ with the specified @newTag@.
-}
addCircleArcAs ::
  MonadIO m =>
  -- | start
  Point ->
  -- |  center
  Point ->
  -- |  end
  Point ->
  -- | newTag
  CurveTag ->
  GmshT m Curve
addCircleArcAs (Entity begin) (Entity center) (Entity end) (EntityTag newTag)
  | newTag < 0 = error "newTag must be non-negative!"
  | otherwise =
    fmap (Entity @CurveE) $
      embedLow $
        Low.gmshModelGeoAddCircleArc begin center end newTag (Vec3 0 0 0)

data CircleArcConfig = CircleArc
  { -- | Circle start point
    circleStart :: !Point
  , -- | The center of the circle
    circleCenter :: !Point
  , -- | Circle end point
    circleEnd :: !Point
  , -- | If positive, set the tag explicitly to that value.
    circleNewTag :: Maybe CurveTag
  , -- | Explicitly set the plane of the circle arc when specified non-zero.
    circleNormal :: Maybe Vec3
  }
  deriving (Show, Eq, Ord, Generic, Typeable)

addCircleArc' :: MonadIO m => CircleArcConfig -> GmshT m Curve
addCircleArc' CircleArc {..} =
  fmap (Entity @CurveE) $
    embedLow $
      Low.gmshModelGeoAddCircleArc
        (rawEntityTag circleStart)
        (rawEntityTag circleCenter)
        (rawEntityTag circleEnd)
        (maybe (-1) getRawEntityTag circleNewTag)
        (fromMaybe (Vec3 0 0 0) circleNormal)

data EllipseArcConfig = EllipseArc
  { ellipseStart :: !Point
  , ellipseCenter :: !Point
  , ellipseMajor :: !Point
  , ellipseEnd :: !Point
  , ellipseNewTag :: Maybe CurveTag
  , ellipseNormal :: !Vec3
  }

addEllipseArc ::
  MonadIO m =>
  -- | start
  Point ->
  -- | center
  Point ->
  -- | major
  Point ->
  -- | end
  Point ->
  GmshT m Curve
addEllipseArc ellipseStart ellipseCenter ellipseMajor ellipseEnd =
  addEllipseArc'
    EllipseArc {ellipseNewTag = Nothing, ellipseNormal = Vec3 0 0 0, ..}

addEllipseArcOn ::
  MonadIO m =>
  -- | start
  Point ->
  -- | center
  Point ->
  -- | major
  Point ->
  -- | end
  Point ->
  -- | (nx, ny, nz)
  Vec3 ->
  GmshT m Curve
addEllipseArcOn ellipseStart ellipseCenter ellipseMajor ellipseEnd ellipseNormal =
  addEllipseArc'
    EllipseArc {ellipseNewTag = Nothing, ..}

addEllipseArcAs ::
  MonadIO m =>
  -- | start
  Point ->
  -- |  center
  Point ->
  -- |  major
  Point ->
  -- |  end
  Point ->
  -- | newTag
  CurveTag ->
  GmshT m Curve
addEllipseArcAs ellipseStart ellipseCenter ellipseMajor ellipseEnd new@(EntityTag newTag)
  | newTag < 0 = error "newTag must be non-negative!"
  | otherwise =
    addEllipseArc'
      EllipseArc {ellipseNewTag = Just new, ellipseNormal = Vec3 0 0 0, ..}

{- |
Add an ellipse arc (strictly smaller than Pi) in the built-in CAD
representation, between the two points @startTag@ and @endTag@, and with
center @centerTag@ and major axis point @majorTag@. If @tag@ is positive,
set the tag explicitly; otherwise a new tag is selected automatically. If
(@nx@, @ny@, @nz@) != (0, 0, 0), explicitly set the plane of the circle
arc. Return the tag of the ellipse arc.
-}
addEllipseArc' :: MonadIO m => EllipseArcConfig -> GmshT m Curve
addEllipseArc' EllipseArc {..} =
  fmap Entity $
    embedLow $
      gmshModelGeoAddEllipseArc
        (rawEntityTag ellipseStart)
        (rawEntityTag ellipseCenter)
        (rawEntityTag ellipseMajor)
        (rawEntityTag ellipseEnd)
        (maybe (-1) getRawEntityTag ellipseNewTag)
        ellipseNormal

{- |
Add a Bezier curve in the built-in CAD representation, with @points@
control points. If @tag@ is positive, set the tag explicitly; otherwise a
new tag is selected automatically. Return the tag of the Bezier curve.
-}
addBezierAs ::
  (MonadIO m, Foldable t) =>
  t Point ->
  CurveTag ->
  GmshT m Curve
addBezierAs points tag =
  fmap Entity $
    embedLow $
      gmshModelGeoAddBezier (L.fold (L.premap rawEntityTag L.vector) points) (getRawEntityTag tag)

{- |
Add a Bezier curve in the built-in CAD representation, with @points@
control points. Return the tag of the Bezier curve.
-}
addBezier ::
  (MonadIO m, Foldable t) =>
  t Point ->
  GmshT m Curve
addBezier = (`addBezierAs` (-1))

{- |
Add a spline (C2 b-spline) curve in the built-in CAD representation,
going through the points @points@. If @tag@ is positive, set the tag
explicitly; otherwise a new tag is selected automatically. Create a
periodic curve if the first and last points are the same. Return the tag of
the spline curve.
-}
addSplineAs ::
  (MonadIO m, Foldable t) =>
  t Point ->
  CurveTag ->
  GmshT m Curve
addSplineAs points tag =
  fmap Entity $
    embedLow $
      gmshModelGeoAddSpline (L.fold (L.premap rawEntityTag L.vector) points) (getRawEntityTag tag)

{- |
Add a spline (C2 b-spline) curve in the built-in CAD representation,
going through the points @points@. Create a periodic curve if the first
and last points are the same. Return the tag of the spline curve.
-}
addSpline ::
  (MonadIO m, Foldable t) =>
  t Point ->
  GmshT m Curve
addSpline = (`addSplineAs` (-1))

{- |
Add a curve loop (a closed wire) in the built-in CAD representation, formed
by the given curves, reorienting curves if necessary.
-}
addCurveLoop ::
  MonadIO m => Foldable t => t Curve -> GmshT m CurveLoop
addCurveLoop curves =
  fmap Entity $
    embedLow $
      Low.gmshModelGeoAddCurveLoop
        (L.fold (L.premap rawEntityTag L.vector) curves)
        (-1)
        True

data CurveLoopConfig = CurveLoop
  { clCurves :: V.Vector Curve
  , clNewTag :: Maybe CurveLoopTag
  , clReorient :: Bool
  }
  deriving (Show, Eq, Ord, Generic, Typeable)

addCurveLoop' :: MonadIO m => CurveLoopConfig -> GmshT m (Entity CurveLoopE)
addCurveLoop' CurveLoop {..} =
  fmap Entity $
    embedLow $
      Low.gmshModelGeoAddCurveLoop
        (V.convert $ coerce @_ @(V.Vector Low.EntityTag) clCurves)
        (maybe (-1) getRawEntityTag clNewTag)
        clReorient

addPlaneSurface :: (MonadIO m, Foldable t) => t CurveLoop -> GmshT m Surface
addPlaneSurface curves =
  fmap Entity $
    embedLow $ Low.gmshModelGeoAddPlaneSurface (L.fold (L.premap coerce L.vector) curves) (-1)

addPlaneSurfaceAs ::
  (MonadIO m, Foldable t) =>
  t CurveLoop ->
  SurfaceTag ->
  GmshT m Surface
addPlaneSurfaceAs curves clTag =
  fmap Entity $
    embedLow $ Low.gmshModelGeoAddPlaneSurface (L.fold (L.premap coerce L.vector) curves) (coerce clTag)

{- |
Extrude the entities @entities@ in the built-in CAD representation, using a
translation along @axis@ and returns the extruded entities

See also 'extrude'' for the variant accepting 'SomeEntity'.
-}
extrude ::
  (MonadIO m, Foldable t, HasDimension entity) =>
  t (Entity entity) ->
  -- | @axis@
  Vec3 ->
  -- | If non-empty, also extrude the mesh: the
  --   entries give the number of elements in each layer.
  [Int] ->
  -- | If non-empty, it provides the (cumulative) height of the different
  -- layers, normalized to 1.
  [Double] ->
  -- | 'True', if recombine the mesh in the layers.
  Bool ->
  -- | See 'fromSomeEntity' and 'classifyEntities' for decode.
  GmshT m (V.Vector SomeEntity)
extrude = extrude' . L.fold (L.premap MkSomeEntity L.list)

{- |
Extrude the entities @entities@ in the built-in CAD representation, using a
translation along @axis@ and returns the extruded entities

See also 'extrude' for the variant to extrude entities of the same kind.
-}
extrude' ::
  (MonadIO m, Foldable t) =>
  t SomeEntity ->
  -- | @axis@
  Vec3 ->
  -- | If non-empty, also extrude the mesh: the
  --   entries give the number of elements in each layer.
  [Int] ->
  -- | If non-empty, it provides the (cumulative) height of the different
  -- layers, normalized to 1.
  [Double] ->
  -- | 'True', if recombine the mesh in the layers.
  Bool ->
  -- | See 'fromSomeEntity' and 'classifyEntities' for decode.
  GmshT m (V.Vector SomeEntity)
extrude' entities axis layers heights recombine
  | length heights /= length layers =
    error $
      "The number of layer and heights number mismatch: "
        <> show (length heights, length layers)
  | otherwise = do
    let ents = L.fold (L.premap someToRawEntity L.vector) entities
    extrudeds <-
      embedLow $
        Low.gmshModelGeoExtrude
          ents
          axis
          (L.fold (L.premap fromIntegral L.vector) layers)
          (L.fold (L.premap realToFrac L.vector) heights)
          recombine
    pure $ V.map toSomeEntity $ S.convert extrudeds

{- |
Extrude the entities @entities@ in the OpenCASCADE CAD representation, using a
translation along @axis@ and returns the extruded entities as HKD without Laterals.

See also 'extrude' for the variant to extrude entities of the same kind, 'extude'' for accepting 'SomeEntity', and 'extrudeWithLateralsB' for the vesion with laterals.
-}
extrudeNoLateralB ::
  ( AllB Extrusible h
  , MonadUnliftIO m
  , Traversable t
  , TraversableB h
  , ConstraintsB h
  ) =>
  h t ->
  Vec3 ->
  [Int] ->
  [Double] ->
  GmshT m (h (Compose t Extrusion))
extrudeNoLateralB = withExtruderNoLateral extrude'

{- |
Extrude the entities @entities@ in the OpenCASCADE CAD representation, using a
translation along @axis@ and returns the extruded entities as HKD without Laterals.

See also 'extrude' for the variant to extrude entities of the same kind, 'extude'' for accepting 'SomeEntity', and 'extrudeNoLateralB' for the vesion without laterals.
-}
extrudeWithLateralsB ::
  ( AllB Extrusible h
  , MonadUnliftIO m
  , Traversable t
  , TraversableB h
  , ConstraintsB h
  ) =>
  h t ->
  Vec3 ->
  [Int] ->
  [Double] ->
  GmshT m (h (Compose t Extrusion))
extrudeWithLateralsB = withExtruderWithLateral extrude'

{- |
Extrude the entities  in the built-in CAD representation, using a
rotation of @angle@ radians around the axis of revolution.

See also 'revolve' for the variant to extrude entities of the same kind.
-}
revolve ::
  (MonadIO m, Foldable t, HasDimension ent) =>
  t (Entity ent) ->
  -- | @center@
  Coord ->
  -- | axis direction
  Vec3 ->
  -- | angle (in radian)
  Double ->
  -- | If non-empty, also extrude the mesh: the
  --   entries give the number of elements in each layer.
  [Int] ->
  -- | If non-empty, it provides the (cumulative) height of the different
  -- layers, normalized to 1.
  [Double] ->
  -- | 'True', if recombine the mesh in the layers.
  Bool ->
  -- | See 'fromSomeEntity' and 'classifyEntities' for decode.
  GmshT m (V.Vector SomeEntity)
revolve = revolve' . L.fold (L.premap MkSomeEntity L.list)

{- |
Extrude the entities  in the built-in CAD representation, using a
rotation of @angle@ radians around the axis of revolution.

See also 'revolve' for the variant to extrude entities of the same kind.
-}
revolve' ::
  (MonadIO m, Foldable t) =>
  t SomeEntity ->
  -- | @center@
  Coord ->
  -- | axis direction
  Vec3 ->
  -- | angle (in radian)
  Double ->
  -- | If non-empty, also extrude the mesh: the
  --   entries give the number of elements in each layer.
  [Int] ->
  -- | If non-empty, it provides the (cumulative) height of the different
  -- layers, normalized to 1.
  [Double] ->
  -- | 'True', if recombine the mesh in the layers.
  Bool ->
  -- | See 'fromSomeEntity' and 'classifyEntities' for decode.
  GmshT m (V.Vector SomeEntity)
revolve' entities center axis angle layers heights recombine = do
  let ents = L.fold (L.premap someToRawEntity L.vector) entities
  extrudeds <-
    embedLow $
      Low.gmshModelGeoRevolve
        ents
        center
        axis
        (realToFrac angle)
        recombine
        (L.fold (L.premap fromIntegral L.vector) layers)
        (L.fold (L.premap realToFrac L.vector) heights)
  pure $ V.map toSomeEntity $ S.convert extrudeds

removeAllDuplicates :: MonadIO m => GmshT m ()
removeAllDuplicates = embedLow_ Low.gmshModelGeoRemoveAllDuplicates

{- | Mirror the entities in the OpenCASCADE CAD representation, with
respect to the plane of equation \(a x + b y + c z + d = 0\).

See also: 'mirror'.
-}
mirror' ::
  (Foldable t, MonadIO m) =>
  Double ->
  Double ->
  Double ->
  Double ->
  t SomeEntity ->
  GmshT m ()
mirror' a b c d ents =
  embedLow_ $
    Low.gmshModelGeoMirror
      (L.fold (L.premap someToRawEntity L.vector) ents)
      (realToFrac a)
      (realToFrac b)
      (realToFrac c)
      (realToFrac d)

{- | Mirror the entities in the OpenCASCADE CAD representation, with
respect to the plane of equation \(a x + b y + c z + d = 0\).

See also: 'mirror''.
-}
mirror ::
  (HasDimension e, Foldable t, MonadIO m) =>
  Double ->
  Double ->
  Double ->
  Double ->
  t (Entity e) ->
  GmshT m ()
mirror a b c d =
  mirror' a b c d
    . map MkSomeEntity
    . F.toList

copy' :: (Foldable t, MonadIO m) => t SomeEntity -> GmshT m (V.Vector SomeEntity)
copy' entities =
  V.map toSomeEntity . V.convert
    <$> embedLow (Low.gmshModelGeoCopy (L.fold (L.premap someToRawEntity L.vector) entities))

copy ::
  (MonadIO m, HasDimension e, Foldable t) =>
  t (Entity e) ->
  GmshT m (V.Vector (Entity e))
copy =
  fmap (V.map $ \(MkSomeEntity e) -> coerce e)
    . copy'
    . map MkSomeEntity
    . F.toList
