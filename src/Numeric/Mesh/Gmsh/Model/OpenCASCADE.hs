{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Numeric.Mesh.Gmsh.Model.OpenCASCADE
  ( synchronize,
    removeAllDuplicates,
    addPoint,
    addPointAs,
    addPoint',
    addLine',
    addLine,
    addCircleArcAs,
    addCircleArc,
    addEllipseArcAs,
    addEllipseArc,
    Radian,
    OccCircleConfig (..),
    addCircle',
    defaultCircleOpts,
    addCircle,
    addCircleBetween,
    OccEllipseConfig (..),
    addEllipse',
    defaultEllipseOpts,
    addEllipse,
    addEllipseBetween,
    addBezier,
    addBezierAs,
    addSpline,
    addSplineAs,
    addCurveLoop,
    addCurveLoopAs,
    addPlaneSurfaceAs,
    addPlaneSurface,
    addSurfaceLoop',
    addSurfaceLoop,
    addVolumeAs,
    addVolume,
    CylinderConfig (..),
    defaultCylinderConfig,
    addCylinder',
    addCylinder,
    extrudeWithLateralsB,
    extrudeNoLateralB,
    extrude',
    extrude,
    revolve',
    revolve,
    fuseB,
    fuse',
    fuse,
    intersectB,
    intersect',
    intersect,
    cutB,
    cut',
    cut,
    fragmentB,
    fragment',
    fragment,
    copy',
    copy,
    getBoundingBox,
    getCenterOfMass,
    dilate',
    dilate,
    getEntitiesInBoundingBox',
    getAllEntitiesInBoundingBox,
    getEntitiesInBoundingBox,
    getEntities',
    getAllEntities,
    getEntities,
    getVolumes,
    getSurfaces,
    getCurves,
    getPoints,
    rotate',
    rotate,
    translate',
    translate,
    mirror',
    mirror,
    addBox,
    addBoxAs,
    remove',
    remove,
    addRoundedRectangle,
    addRectangle,
    addRectangle',
    addSurfaceFilling',
    addSurfaceFilling,
  )
where

import Barbies (ConstraintsB (AllB), TraversableB)
import qualified Control.Foldl as L
import Control.Monad.IO.Class (MonadIO)
import Data.Coerce (coerce)
import qualified Data.Foldable as F
import Data.Functor.Compose (Compose)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import GHC.Generics (Generic)
import Numeric.Mesh.Gmsh.Helper
import Numeric.Mesh.Gmsh.LowLevel (gmshModelOccAddBezier, gmshModelOccAddSpline)
import qualified Numeric.Mesh.Gmsh.LowLevel as Low
import Numeric.Mesh.Gmsh.Types
import Type.Reflection (Typeable)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (throwIO)

synchronize :: MonadIO m => GmshT m ()
synchronize = embedLow_ Low.gmshModelOccSynchronize

removeAllDuplicates :: MonadIO m => GmshT m ()
removeAllDuplicates = embedLow_ Low.gmshModelOccRemoveAllDuplicates

{- |
Add a geometrical point in the OpenCASCADE CAD representation, at
coordinates (@x@, @y@, @z@).
-}
addPoint :: MonadIO m => Coord -> GmshT m Point
addPoint coord = fmap Entity $ embedLow $ Low.gmshModelOccAddPoint coord 0.0 (-1)

{- |
Add a geometrical point in the OpenCASCADE CAD representation, at
coordinates (@x@, @y@, @z@).  If @tag@ is positive, set the tag
explicitly; otherwise a new tag is selected automatically.
-}
addPointAs :: MonadIO m => Coord -> PointTag -> GmshT m Point
addPointAs coord (EntityTag tag) = fmap Entity $ embedLow $ Low.gmshModelOccAddPoint coord 0.0 tag

addPoint' :: MonadIO m => PointConfig -> GmshT m Point
addPoint' Point {..} = fmap Entity $ embedLow $ Low.gmshModelOccAddPoint pointCoord (maybe 0.0 realToFrac pointMeshSize) (maybe (-1) coerce pointNewTag)

{- |
Add a straight line segment in the OpenCASCADE CAD representation, between the
two points with tags @start@ and @end@. If @tag@ is positive, set the
tag explicitly; otherwise a new tag is selected automatically. Return the
tag of the line.
-}
addLine' :: MonadIO m => Point -> Point -> CurveTag -> GmshT m Curve
addLine' start end tag =
  fmap Entity $
    embedLow $
      Low.gmshModelOccAddLine (coerce start) (coerce end) (coerce tag)

{- |
Add a straight line segment in the OpenCASCADE CAD representation, between the
two points with tags @start@ and @end@.
-}
addLine :: MonadIO m => Point -> Point -> GmshT m Curve
addLine start end = addLine' start end (-1)

{- |
Add a circle arc in the OpenCASCADE CAD representation, between the
two points with tags @start@ and @end@, with center @center@. If @tag@
is positive, set the tag explicitly; otherwise a new tag is selected
automatically. Return the tag of the circle arc.
-}
addCircleArcAs ::
  MonadIO m =>
  -- | @start@
  Point ->
  -- | @center
  Point ->
  -- | @end@
  Point ->
  -- | @tag@
  CurveTag ->
  GmshT m Curve
addCircleArcAs (Entity start) (Entity center) (Entity end) (EntityTag tag) =
  fmap Entity $
    embedLow $ Low.gmshModelOccAddCircleArc start center end tag

{- |
Add a circle arc in the OpenCASCADE CAD representation, between the
two points with tags @start@ and @end@, with center @center@.
-}
addCircleArc ::
  MonadIO m =>
  -- | @start@
  Point ->
  -- | @center
  Point ->
  -- | @end@
  Point ->
  GmshT m Curve
addCircleArc start center end = addCircleArcAs start center end (-1)

addEllipseArcAs ::
  MonadIO m =>
  -- | @start@
  Point ->
  -- | @center
  Point ->
  -- | @major
  Point ->
  -- | @end@
  Point ->
  -- | @tag@
  CurveTag ->
  GmshT m Curve
addEllipseArcAs (Entity start) (Entity center) (Entity major) (Entity end) (EntityTag tag) =
  fmap Entity $
    embedLow $ Low.gmshModelOccAddEllipseArc start center major end tag

{- |
Add an ellipse arc in the OpenCASCADE CAD representation, between the
two points with tags @start@ and @end@, with center @center@ and major axis point @major@.
-}
addEllipseArc ::
  MonadIO m =>
  -- | @start@
  Point ->
  -- | @center@
  Point ->
  -- | @majoir@
  Point ->
  -- | @end@
  Point ->
  GmshT m Curve
addEllipseArc start center major end = addEllipseArcAs start center major end (-1)

type Radian = Double

data OccCircleConfig = OccCircleConfig
  { circleCenter :: Coord
  , circleRadius :: Double
  , circleNewTag :: Maybe CurveTag
  , -- | When specified, create a circle arc between two angles (in radian).
    circleAngles :: Maybe (Radian, Radian)
  }
  deriving (Show, Eq, Ord, Typeable, Generic)

defaultCircleOpts :: OccCircleConfig
defaultCircleOpts =
  OccCircleConfig
    { circleCenter = Coord $ Vec3 0 0 0
    , circleRadius = 1
    , circleNewTag = Nothing
    , circleAngles = Nothing
    }

{- |
Add a circle of center (@x@, @y@, @z@) and radius @r@ in the OpenCASCADE
CAD representation. If @tag@ is positive, set the tag explicitly; otherwise
a new tag is selected automatically. If @angle1@ and @angle2@ are
specified, create a circle arc between the two angles. Return the tag of
the circle.
-}
addCircle' :: MonadIO m => OccCircleConfig -> GmshT m Curve
addCircle' OccCircleConfig {..} =
  fmap Entity $
    embedLow $
      Low.gmshModelOccAddCircle
        circleCenter
        (realToFrac circleRadius)
        (maybe (-1) coerce circleNewTag)
        (maybe 0 (realToFrac . fst) circleAngles)
        (maybe (2 * pi) (realToFrac . snd) circleAngles)

{- |
Add a circle of center (@x@, @y@, @z@) and radius @r@ in the OpenCASCADE
CAD representation.
-}
addCircle :: MonadIO m => Coord -> Double -> GmshT m Curve
addCircle circleCenter circleRadius =
  addCircle' defaultCircleOpts {circleCenter, circleRadius}

{- |
Add a circle arc of center (@x@, @y@, @z@) and radius @r@ between @angle1@ and @angle2@ in the OpenCASCADE
CAD representation.
-}
addCircleBetween :: MonadIO m => Coord -> Double -> Radian -> Radian -> GmshT m Curve
addCircleBetween circleCenter circleRadius angle1 angle2 =
  addCircle' defaultCircleOpts {circleCenter, circleRadius, circleAngles = Just (angle1, angle2)}

data OccEllipseConfig = OccEllipseConfig
  { ellipseCenter :: Coord
  , ellipseMajorRadius :: Double
  , ellipseMinorRadius :: Double
  , ellipseNewTag :: Maybe CurveTag
  , -- | When specified, create an ellipse arc between two angles (in radian).
    ellipseAngles :: Maybe (Radian, Radian)
  }
  deriving (Show, Eq, Ord, Typeable, Generic)

defaultEllipseOpts :: OccEllipseConfig
defaultEllipseOpts =
  OccEllipseConfig
    { ellipseCenter = Coord $ Vec3 0 0 0
    , ellipseMajorRadius = 1
    , ellipseMinorRadius = 1
    , ellipseNewTag = Nothing
    , ellipseAngles = Nothing
    }

{- |
Add an ellipse of center (@x@, @y@, @z@) and radii @r1@ and @r2@ along the
x- and y-axes, respectively, in the OpenCASCADE CAD representation. If
@tag@ is positive, set the tag explicitly; otherwise a new tag is selected
automatically. If @angle1@ and @angle2@ are specified, create an ellipse
arc between the two angles. Return the tag of the ellipse. Note that
OpenCASCADE does not allow creating ellipses with the major radius (along
the x-axis) smaller than or equal to the minor radius (along the y-axis):
rotate the shape or use 'addCircle' in such cases.
-}
addEllipse' :: MonadIO m => OccEllipseConfig -> GmshT m Curve
addEllipse' OccEllipseConfig {..} =
  fmap Entity $
    embedLow $
      Low.gmshModelOccAddEllipse
        ellipseCenter
        (realToFrac ellipseMajorRadius)
        (realToFrac ellipseMinorRadius)
        (maybe (-1) coerce ellipseNewTag)
        (maybe 0 (realToFrac . fst) ellipseAngles)
        (maybe (2 * pi) (realToFrac . snd) ellipseAngles)

{- |
Add an ellipse of center (@x@, @y@, @z@), major radius @r1@, and minor radius @r2@
in the OpenCASCADE CAD representation.
Note that OpenCASCADE does not allow creating ellipses with the major radius (along
the x-axis) smaller than or equal to the minor radius (along the y-axis):
rotate the shape or use 'addCircle' in such cases.
-}
addEllipse :: MonadIO m => Coord -> Double -> Double -> GmshT m Curve
addEllipse ellipseCenter ellipseMajorRadius ellipseMinorRadius =
  addEllipse' defaultEllipseOpts {ellipseCenter, ellipseMajorRadius, ellipseMinorRadius}

{- |
Add an ellipse arc of center (@x@, @y@, @z@) and major and minorradius @r1, r2@ between @angle1@ and @angle2@ in the OpenCASCADE
CAD representation.
Note that OpenCASCADE does not allow creating ellipses with the major radius (along
the x-axis) smaller than or equal to the minor radius (along the y-axis):
rotate the shape or use 'addCircle' in such cases.
-}
addEllipseBetween :: MonadIO m => Coord -> Double -> Double -> Radian -> Radian -> GmshT m Curve
addEllipseBetween ellipseCenter ellipseMajorRadius ellipseMinorRadius angle1 angle2 =
  addEllipse'
    defaultEllipseOpts
      { ellipseCenter
      , ellipseMajorRadius
      , ellipseMinorRadius
      , ellipseAngles = Just (angle1, angle2)
      }

{- |
Add a Bezier curve in the OpenCASCADE CAD representation, with @points@
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
      gmshModelOccAddBezier (L.fold (L.premap rawEntityTag L.vector) points) (getRawEntityTag tag)

{- |
Add a Bezier curve in the OpenCASCADE CAD representation, with @points@
control points. Return the tag of the Bezier curve.
-}
addBezier ::
  (MonadIO m, Foldable t) =>
  t Point ->
  GmshT m Curve
addBezier = (`addBezierAs` (-1))

{- |
Add a spline (C2 b-spline) curve in the OpenCASCADE CAD representation,
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
      gmshModelOccAddSpline (L.fold (L.premap rawEntityTag L.vector) points) (getRawEntityTag tag)

{- |
Add a spline (C2 b-spline) curve in the OpenCASCADE CAD representation,
going through the points @points@. Create a periodic curve if the first
and last points are the same. Return the tag of the spline curve.
-}
addSpline ::
  (MonadIO m, Foldable t) =>
  t Point ->
  GmshT m Curve
addSpline = (`addSplineAs` (-1))

{- |
Add a curve loop (a closed wire) in the OpenCASCADE representation, formed
by the curves @curveTags@. @curveTags@ should contain tags of
curves forming a closed loop.
Note that an OpenCASCADE curve loop can be made of
curves that share geometrically identical (but topologically different)
points.
-}
addCurveLoop ::
  MonadIO m => Foldable t => t Curve -> GmshT m CurveLoop
addCurveLoop curves
  | null curves = throwIO $ ValidationError "addCurveLoop: curve loop must be non-empty!"
  | otherwise = addCurveLoopAs curves (-1)

{- |
Add a curve loop (a closed wire) in the OpenCASCADE representation, formed
by the curves @curveTags@. @curveTags@ should contain tags of
curves forming a closed loop.
Note that an OpenCASCADE curve loop can be made of
curves that share geometrically identical (but topologically different)
points.
If @tag@ is positive, set the tag explicitly; otherwise a new
tag is selected automatically.
Return the tag of the curve loop.
-}
addCurveLoopAs :: (Foldable t, MonadIO m) => t Curve -> CurveLoopTag -> GmshT m CurveLoop
addCurveLoopAs curves tag =
  fmap Entity $
    embedLow $
      Low.gmshModelOccAddCurveLoop
        (L.fold (L.premap coerce L.vector) curves)
        (coerce tag)

{- |
Add a plane surface in the OpenCASCADE CAD representation, defined by one
or more curve loops (or closed wires) @wires@. The first curve loop
defines the exterior contour; additional curve loop define holes. If @tag@
is positive, set the tag explicitly; otherwise a new tag is selected
automatically. Return the tag of the surface.
-}
addPlaneSurfaceAs :: (Foldable t, MonadIO m) => t CurveLoop -> SurfaceTag -> GmshT m Surface
addPlaneSurfaceAs wires (EntityTag tag) =
  fmap Entity $
    embedLow $
      Low.gmshModelOccAddPlaneSurface
        (L.fold (L.premap coerce L.vector) wires)
        tag

{- |
Add a plane surface in the OpenCASCADE CAD representation, defined by one
or more curve loops (or closed wires) @wires@. The first curve loop
defines the exterior contour; additional curve loop define holes.
-}
addPlaneSurface :: (Foldable t, MonadIO m) => t CurveLoop -> GmshT m Surface
addPlaneSurface = (`addPlaneSurfaceAs` (-1))

{- |
Add a surface loop (a closed shell) in the OpenCASCADE CAD representation,
formed by @surfaces@.  If @tag@ is positive, set the tag explicitly;
otherwise a new tag is selected automatically.
Setting @sewing@ allows to build a shell made of surfaces
that share geometrically identical (but topologically different) curves.
-}
addSurfaceLoop' ::
  (Foldable t, MonadIO m) =>
  t Surface ->
  SurfaceLoopTag ->
  -- | @sewing@
  Bool ->
  GmshT m SurfaceLoop
addSurfaceLoop' surfaces (EntityTag tag) sewing =
  fmap Entity $
    embedLow $
      Low.gmshModelOccAddSurfaceLoop
        (L.fold (L.premap coerce L.vector) surfaces)
        tag
        sewing

{- |
Add a surface loop (a closed shell) in the OpenCASCADE CAD representation,
formed by @surfaces@, allowing to build a shell made of surfaces
that share geometrically identical (but topologically different) curves.
-}
addSurfaceLoop ::
  (Foldable t, MonadIO m) =>
  t Surface ->
  GmshT m SurfaceLoop
addSurfaceLoop surfaces = addSurfaceLoop' surfaces (-1) True

{- |
Add a volume (a region) in the OpenCASCADE CAD representation, defined by one
or more surface loops @shellTags@. The first surface loop defines the exterior
boundary; additional surface loop define holes. If @tag@ is positive, set
the tag explicitly; otherwise a new tag is selected automatically. Return
the tag of the volume.
-}
addVolumeAs ::
  (MonadIO m, Foldable t) =>
  t SurfaceLoop ->
  EntityTag e ->
  GmshT m Volume
addVolumeAs shells (EntityTag tag) =
  fmap Entity $
    embedLow $
      Low.gmshModelOccAddVolume (L.fold (L.premap coerce L.vector) shells) tag

addVolume :: (MonadIO m, Foldable t) => t SurfaceLoop -> GmshT m Volume
addVolume = (`addVolumeAs` (-1))

data CylinderConfig = CylinderConfig
  { -- | center of the cylinder
    cylCenter :: Coord
  , -- | axis of the cylinder
    cylAxis :: Vec3
  , cylRadius :: Double
  , cylNewTag :: Maybe VolumeTag
  , -- |angular opening
    cylOpenAngle :: Maybe Radian
  }
  deriving (Show, Eq, Ord, Typeable, Generic)

{- |
Add a cylinder in the OpenCASCADE CAD representation, defined by the center
(@x@, @y@, @z@) of its first circular face, the 3 components (@dx@, @dy@,
@dz@) of the vector defining its axis and its radius @r@. The optional
@angle@ argument defines the angular opening (from 0 to 2*Pi). If @tag@ is
positive, set the tag explicitly; otherwise a new tag is selected
automatically. Return the tag of the cylinder.
-}
addCylinder' :: MonadIO m => CylinderConfig -> GmshT m Volume
addCylinder' CylinderConfig {..} =
  fmap (Entity @VolumeE) $
    embedLow $
      Low.gmshModelOccAddCylinder
        cylCenter
        cylAxis
        (realToFrac cylRadius)
        (maybe (-1) getRawEntityTag cylNewTag)
        (maybe (2 * pi) realToFrac cylOpenAngle)

-- | Adds OpenCASCADE Cylinder volume with the given center, axis and radius.
addCylinder ::
  MonadIO m =>
  Coord ->
  Vec3 ->
  -- | radius
  Double ->
  GmshT m Volume
addCylinder cylCenter cylAxis cylRadius =
  addCylinder' defaultCylinderConfig {cylCenter, cylAxis, cylRadius}

defaultCylinderConfig :: CylinderConfig
defaultCylinderConfig =
  CylinderConfig
    { cylCenter = Coord $ Vec3 0 0 0
    , cylAxis = Vec3 0 0 1
    , cylRadius = 1
    , cylNewTag = Nothing
    , cylOpenAngle = Nothing
    }

{- |
Extrude the entities @entities@ in the OpenCASCADE CAD representation, using a
translation along @axis@ and returns the extruded entities

See also 'extrude' for the variant to extrude entities of the same kind and 'extrudeNoLateralB' for the one returns extruded entities as HKD without laterals.
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
        Low.gmshModelOccExtrude
          ents
          axis
          (L.fold (L.premap fromIntegral L.vector) layers)
          (L.fold (L.premap realToFrac L.vector) heights)
          recombine
    pure $ V.map toSomeEntity $ S.convert extrudeds

{- |
Extrude the entities @entities@ in the OpenCASCADE CAD representation, using a
translation along @axis@ and returns the extruded entities

See also 'extrude'' for the variant accepting 'SomeEntity' and 'extrudeNoLateralB' for the one returns extruded entities as HKD without laterals.
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
Extrude the entities  in the OpenCASCADE CAD representation, using a
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
      Low.gmshModelOccRevolve
        ents
        center
        axis
        (realToFrac angle)
        recombine
        (L.fold (L.premap fromIntegral L.vector) layers)
        (L.fold (L.premap realToFrac L.vector) heights)
  pure $ V.map toSomeEntity $ S.convert extrudeds

{- |
Extrude the entities  in the OpenCASCADE CAD representation, using a
rotation of @angle@ radians around the axis of revolution.

See also 'revolve'' for the variant to extrude 'SomeEntity'.
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
Compute the boolean union (the fusion) between the entities @objects@ and
@tools@ in the OpenCASCADE CAD representation. If @tag@ is positive, try to set the tag
explicitly (only valid if the boolean operation results in a single
entity). Remove the object if @removeObject@ is set. Remove the tool if
@removeTool@ is set.

Return the resulting entities in @outEntities@ and parent-child dictionaryAs @subEntityDic@.
@subEntityDic@ orders @objects@ and then @tools@, associating their child entities as a vector.

See also 'fuse' for the variant to extrude entities of the same kind and 'fuseB' for the higher-kinded version.
-}
fuse' ::
  (MonadIO m, Foldable t, Foldable t') =>
  -- | objects
  t SomeEntity ->
  -- | tools
  t' SomeEntity ->
  -- | tag
  Low.EntityTag ->
  -- | removeObject
  Bool ->
  -- | removeTool
  Bool ->
  -- | @(outEntities, subEntityDic)@
  GmshT m (V.Vector SomeEntity, V.Vector (V.Vector SomeEntity))
fuse' objects tools tag removeObject removeTool = do
  (outEnts, entityDic) <-
    embedLow $
      Low.gmshModelOccFuse
        (L.fold (L.premap someToRawEntity L.vector) objects)
        (L.fold (L.premap someToRawEntity L.vector) tools)
        tag
        removeObject
        removeTool
  pure
    ( V.map toSomeEntity $ V.convert outEnts
    , V.map (V.map toSomeEntity . V.convert) entityDic
    )

{- |
Compute the boolean union (the fusion) between the entities @objects@ and
@tools@ in the OpenCASCADE CAD representation. If @tag@ is positive, try to set the tag
explicitly (only valid if the boolean operation results in a single
entity). Remove the object if @removeObject@ is set. Remove the tool if
@removeTool@ is set.

Return the resulting entities in @outEntities@ and parent-child dictionaryAs @subEntityDic@.
@subEntityDic@ orders @objects@ and then @tools@, associating their child entities as a vector.

See also 'fuse'' for the variant accepting 'SomeEntity' and 'fuseB' for the higher-kinded version.
-}
fuse ::
  ( MonadIO m
  , Foldable t
  , Foldable t'
  , HasDimension e1
  , HasDimension e2
  ) =>
  -- | objects
  t (Entity e1) ->
  -- | tools
  t' (Entity e2) ->
  -- | tag
  Low.EntityTag ->
  -- | removeObject
  Bool ->
  -- | removeTool
  Bool ->
  -- | @(outEntities, subEntityDic)@
  GmshT m (V.Vector SomeEntity, V.Vector (V.Vector SomeEntity))
fuse objects tools tag removeObject removeTool = do
  (outEnts, entityDic) <-
    embedLow $
      Low.gmshModelOccFuse
        (L.fold (L.premap toRawEntity L.vector) objects)
        (L.fold (L.premap toRawEntity L.vector) tools)
        tag
        removeObject
        removeTool
  pure
    ( V.map toSomeEntity $ V.convert outEnts
    , V.map (V.map toSomeEntity . V.convert) entityDic
    )

{- |
Compute the boolean union (the fusion) between the entities @objects@ and
@tools@ in the OpenCASCADE CAD representation. If @tag@ is positive, try to set the tag
explicitly (only valid if the boolean operation results in a single
entity). Remove the object if @removeObject@ is set. Remove the tool if
@removeTool@ is set.

Return the resulting entities in @outEntities@ and parent-child dictionary as wrapped in HKD.

See also 'fuse'' for the variant accepting 'SomeEntity' and 'fuse' for the version accepting the entities of the same dimension.
-}
fuseB ::
  ( AllB OrdinaryEntity objects
  , AllB OrdinaryEntity tools
  , MonadIO m
  , Traversable t
  , Traversable t'
  , ConstraintsB objects
  , ConstraintsB tools
  , TraversableB objects
  , TraversableB tools
  ) =>
  objects t ->
  tools t' ->
  GmshT
    m
    ( V.Vector SomeEntity
    , objects (Compose t V.Vector)
    , tools (Compose t' V.Vector)
    )
fuseB = withBooleanOp fuse'

{- |
Compute the boolean intersection (common parts) between the entities @objects@ and
@tools@ in the OpenCASCADE CAD representation. If @tag@ is positive, try to set the tag
explicitly (only valid if the boolean operation results in a single
entity). Remove the object if @removeObject@ is set. Remove the tool if
@removeTool@ is set.

Return the resulting entities in @outEntities@ and parent-child dictionaryAs @subEntityDic@.
@subEntityDic@ orders @objects@ and then @tools@, associating their child entities as a vector.

See also 'intersect' for the variant to extrude entities of the same kind and 'intersectB' for the higher-kinded version.
-}
intersect' ::
  (MonadIO m, Foldable t, Foldable t') =>
  -- | objects
  t SomeEntity ->
  -- | tools
  t' SomeEntity ->
  -- | tag
  Low.EntityTag ->
  -- | removeObject
  Bool ->
  -- | removeTool
  Bool ->
  -- | @(outEntities, subEntityDic)@
  GmshT m (V.Vector SomeEntity, V.Vector (V.Vector SomeEntity))
intersect' objects tools tag removeObject removeTool = do
  (outEnts, entityDic) <-
    embedLow $
      Low.gmshModelOccIntersect
        (L.fold (L.premap someToRawEntity L.vector) objects)
        (L.fold (L.premap someToRawEntity L.vector) tools)
        tag
        removeObject
        removeTool
  pure
    ( V.map toSomeEntity $ V.convert outEnts
    , V.map (V.map toSomeEntity . V.convert) entityDic
    )

{- |
Compute the boolean union (the fusion) between the entities @objects@ and
@tools@ in the OpenCASCADE CAD representation. If @tag@ is positive, try to set the tag
explicitly (only valid if the boolean operation results in a single
entity). Remove the object if @removeObject@ is set. Remove the tool if
@removeTool@ is set.

Return the resulting entities in @outEntities@ and parent-child dictionaryAs @subEntityDic@.
@subEntityDic@ orders @objects@ and then @tools@, associating their child entities as a vector.

See also 'intersect'' for the variant accepting 'SomeEntity' and 'intersectB' for the higher-kinded version.
-}
intersect ::
  ( MonadIO m
  , Foldable t
  , Foldable t'
  , HasDimension e1
  , HasDimension e2
  ) =>
  -- | objects
  t (Entity e1) ->
  -- | tools
  t' (Entity e2) ->
  -- | tag
  Low.EntityTag ->
  -- | removeObject
  Bool ->
  -- | removeTool
  Bool ->
  -- | @(outEntities, subEntityDic)@
  GmshT m (V.Vector SomeEntity, V.Vector (V.Vector SomeEntity))
intersect objects tools tag removeObject removeTool = do
  (outEnts, entityDic) <-
    embedLow $
      Low.gmshModelOccIntersect
        (L.fold (L.premap toRawEntity L.vector) objects)
        (L.fold (L.premap toRawEntity L.vector) tools)
        tag
        removeObject
        removeTool
  pure
    ( V.map toSomeEntity $ V.convert outEnts
    , V.map (V.map toSomeEntity . V.convert) entityDic
    )

{- |
Compute the boolean intersection (common parts) between the entities @objects@ and
@tools@ in the OpenCASCADE CAD representation. If @tag@ is positive, try to set the tag
explicitly (only valid if the boolean operation results in a single
entity). Remove the object if @removeObject@ is set. Remove the tool if
@removeTool@ is set.

Return the resulting entities in @outEntities@ and parent-child dictionary as wrapped in HKD.

See also 'intersect'' for the variant accepting 'SomeEntity' and 'intersect' for the version accepting the entities of the same dimension.
-}
intersectB ::
  ( AllB OrdinaryEntity objects
  , AllB OrdinaryEntity tools
  , MonadIO m
  , Traversable t
  , Traversable t'
  , ConstraintsB objects
  , ConstraintsB tools
  , TraversableB objects
  , TraversableB tools
  ) =>
  objects t ->
  tools t' ->
  GmshT
    m
    ( V.Vector SomeEntity
    , objects (Compose t V.Vector)
    , tools (Compose t' V.Vector)
    )
intersectB = withBooleanOp intersect'

{- |
Compute the boolean difference between the entities @objects@ and
@tools@ in the OpenCASCADE CAD representation. If @tag@ is positive, try to set the tag
explicitly (only valid if the boolean operation results in a single
entity). Remove the object if @removeObject@ is set. Remove the tool if
@removeTool@ is set.

Return the resulting entities in @outEntities@ and parent-child dictionaryAs @subEntityDic@.
@subEntityDic@ orders @objects@ and then @tools@, associating their child entities as a vector.

See also 'cut' for the variant to extrude entities of the same kind and 'cutB' for the higher-kinded version.
-}
cut' ::
  (MonadIO m, Foldable t, Foldable t') =>
  -- | objects
  t SomeEntity ->
  -- | tools
  t' SomeEntity ->
  -- | tag
  Low.EntityTag ->
  -- | removeObject
  Bool ->
  -- | removeTool
  Bool ->
  -- | @(outEntities, subEntityDic)@
  GmshT m (V.Vector SomeEntity, V.Vector (V.Vector SomeEntity))
cut' objects tools tag removeObject removeTool = do
  (outEnts, entityDic) <-
    embedLow $
      Low.gmshModelOccCut
        (L.fold (L.premap someToRawEntity L.vector) objects)
        (L.fold (L.premap someToRawEntity L.vector) tools)
        tag
        removeObject
        removeTool
  pure
    ( V.map toSomeEntity $ V.convert outEnts
    , V.map (V.map toSomeEntity . V.convert) entityDic
    )

{- |
Compute the boolean diference between the entities @objects@ and
@tools@ in the OpenCASCADE CAD representation. If @tag@ is positive, try to set the tag
explicitly (only valid if the boolean operation results in a single
entity). Remove the object if @removeObject@ is set. Remove the tool if
@removeTool@ is set.

Return the resulting entities in @outEntities@ and parent-child dictionaryAs @subEntityDic@.
@subEntityDic@ orders @objects@ and then @tools@, associating their child entities as a vector.

See also 'cut'' for the variant accepting 'SomeEntity' and 'cutB' for the higher-kinded version.
-}
cut ::
  ( MonadIO m
  , Foldable t
  , Foldable t'
  , HasDimension e1
  , HasDimension e2
  ) =>
  -- | objects
  t (Entity e1) ->
  -- | tools
  t' (Entity e2) ->
  -- | tag
  Low.EntityTag ->
  -- | removeObject
  Bool ->
  -- | removeTool
  Bool ->
  -- | @(outEntities, subEntityDic)@
  GmshT m (V.Vector SomeEntity, V.Vector (V.Vector SomeEntity))
cut objects tools tag removeObject removeTool = do
  (outEnts, entityDic) <-
    embedLow $
      Low.gmshModelOccCut
        (L.fold (L.premap toRawEntity L.vector) objects)
        (L.fold (L.premap toRawEntity L.vector) tools)
        tag
        removeObject
        removeTool
  pure
    ( V.map toSomeEntity $ V.convert outEnts
    , V.map (V.map toSomeEntity . V.convert) entityDic
    )

{- |
Compute the boolean difference between the entities @objects@ and
@tools@ in the OpenCASCADE CAD representation. If @tag@ is positive, try to set the tag
explicitly (only valid if the boolean operation results in a single
entity). Remove the object if @removeObject@ is set. Remove the tool if
@removeTool@ is set.

Return the resulting entities in @outEntities@ and parent-child dictionary as wrapped in HKD.

See also 'cut'' for the variant accepting 'SomeEntity' and 'cut' for the version accepting the entities of the same dimension.
-}
cutB ::
  ( AllB OrdinaryEntity objects
  , AllB OrdinaryEntity tools
  , MonadIO m
  , Traversable t
  , Traversable t'
  , ConstraintsB objects
  , ConstraintsB tools
  , TraversableB objects
  , TraversableB tools
  ) =>
  objects t ->
  tools t' ->
  GmshT
    m
    ( V.Vector SomeEntity
    , objects (Compose t V.Vector)
    , tools (Compose t' V.Vector)
    )
cutB = withBooleanOp cut'

{- |
Compute the boolean fragments (general fuse) resulting from the
intersection of the entities @objects@ and
@tools@ in the OpenCASCADE CAD representation, making all interfaces conformal. When
applied to entities of different dimensions, the lower dimensional entities
will be automatically embedded in the higher dimensional entities if they
are not on their boundary.
If @tag@ is positive, try to set the tag
explicitly (only valid if the boolean operation results in a single
entity). Remove the object if @removeObject@ is set. Remove the tool if
@removeTool@ is set.

See also 'fragment' for the variant to extrude entities of the same kind and 'fragmentB' for the higher-kinded version.
-}
fragment' ::
  (MonadIO m, Foldable t, Foldable t') =>
  -- | objects
  t SomeEntity ->
  -- | tools
  t' SomeEntity ->
  -- | tag
  Low.EntityTag ->
  -- | removeObject
  Bool ->
  -- | removeTool
  Bool ->
  -- | @(outEntities, subEntityDic)@
  GmshT m (V.Vector SomeEntity, V.Vector (V.Vector SomeEntity))
fragment' objects tools tag removeObject removeTool = do
  (outEnts, entityDic) <-
    embedLow $
      Low.gmshModelOccFragment
        (L.fold (L.premap someToRawEntity L.vector) objects)
        (L.fold (L.premap someToRawEntity L.vector) tools)
        tag
        removeObject
        removeTool
  pure
    ( V.map toSomeEntity $ V.convert outEnts
    , V.map (V.map toSomeEntity . V.convert) entityDic
    )

{- |
Compute the boolean diference between the entities @objects@ and
@tools@ in the OpenCASCADE CAD representation. If @tag@ is positive, try to set the tag
explicitly (only valid if the boolean operation results in a single
entity). Remove the object if @removeObject@ is set. Remove the tool if
@removeTool@ is set.Compute the boolean fragments (general fuse) resulting from the
intersection of the entities @objects@ and
@tools@ in the OpenCASCADE CAD representation, making all interfaces conformal. When
applied to entities of different dimensions, the lower dimensional entities
will be automatically embedded in the higher dimensional entities if they
are not on their boundary.
If @tag@ is positive, try to set the tag
explicitly (only valid if the boolean operation results in a single
entity). Remove the object if @removeObject@ is set. Remove the tool if
@removeTool@ is set.

Return the resulting entities in @outEntities@ and parent-child dictionaryAs @subEntityDic@.
@subEntityDic@ orders @objects@ and then @tools@, associating their child entities as a vector.

See also 'fragment'' for the variant accepting 'SomeEntity' and 'fragmentB' for the higher-kinded version.
-}
fragment ::
  ( MonadIO m
  , Foldable t
  , Foldable t'
  , HasDimension e
  ) =>
  -- | objects
  t (Entity e) ->
  -- | tools
  t' (Entity e) ->
  -- | tag
  Low.EntityTag ->
  -- | removeObject
  Bool ->
  -- | removeTool
  Bool ->
  -- | @(outEntities, subEntityDic)@
  GmshT m (V.Vector (Entity e), V.Vector (V.Vector (Entity e)))
fragment objects tools tag removeObject removeTool = do
  (outEnts, entityDic) <-
    embedLow $
      Low.gmshModelOccFragment
        (L.fold (L.premap toRawEntity L.vector) objects)
        (L.fold (L.premap toRawEntity L.vector) tools)
        tag
        removeObject
        removeTool
  pure
    ( V.map (Entity . Low.entityTag) $ V.convert outEnts
    , V.map (V.map (Entity . Low.entityTag) . V.convert) entityDic
    )

{- |
Compute the boolean fragments (general fuse) resulting from the
intersection of the entities @objects@ and
@tools@ in the OpenCASCADE CAD representation, making all interfaces conformal. When
applied to entities of different dimensions, the lower dimensional entities
will be automatically embedded in the higher dimensional entities if they
are not on their boundary.

Return the resulting entities in @outEntities@ and parent-child dictionary as wrapped in HKD.

See also 'fragment'' for the variant accepting 'SomeEntity' and 'fragment' for the version accepting the entities of the same dimension.
-}
fragmentB ::
  ( AllB
      OrdinaryEntity
      objects
  , AllB
      OrdinaryEntity
      tools
  , MonadIO m
  , Traversable t
  , Traversable t'
  , ConstraintsB objects
  , ConstraintsB tools
  , TraversableB objects
  , TraversableB
      tools
  ) =>
  objects t ->
  tools t' ->
  GmshT
    m
    ( V.Vector SomeEntity
    , objects (Compose t V.Vector)
    , tools (Compose t' V.Vector)
    )
fragmentB = withBooleanOp fragment'

copy' :: (Foldable t, MonadIO m) => t SomeEntity -> GmshT m (V.Vector SomeEntity)
copy' entities =
  V.map toSomeEntity . V.convert
    <$> embedLow (Low.gmshModelOccCopy (L.fold (L.premap someToRawEntity L.vector) entities))

copy ::
  (MonadIO m, HasDimension e, Foldable t) =>
  t (Entity e) ->
  GmshT m (V.Vector (Entity e))
copy =
  fmap (V.map $ \(MkSomeEntity e) -> coerce e)
    . copy'
    . map MkSomeEntity
    . F.toList

getAllEntities :: MonadIO m => GmshT m (V.Vector SomeEntity)
getAllEntities = getEntities' (-1)

getEntities :: forall ent m. (HasDimension ent, MonadIO m) => GmshT m (V.Vector (Entity ent))
getEntities =
  V.map (\(MkSomeEntity e) -> coerce e)
    <$> getEntities' (entityDim @ent Proxy)

getVolumes :: MonadIO m => GmshT m (V.Vector Volume)
getVolumes = getEntities @VolumeE

getSurfaces :: MonadIO m => GmshT m (V.Vector Surface)
getSurfaces = getEntities @SurfaceE

getCurves :: MonadIO m => GmshT m (V.Vector Curve)
getCurves = getEntities @CurveE

getPoints :: MonadIO m => GmshT m (V.Vector Point)
getPoints = getEntities @PointE

getEntities' :: MonadIO m => Dimension -> GmshT m (V.Vector SomeEntity)
getEntities' =
  fmap (V.map toSomeEntity . V.convert)
    . embedLow
    . Low.gmshModelOccGetEntities

getAllEntitiesInBoundingBox ::
  MonadIO m => Coord -> Coord -> GmshT m (V.Vector SomeEntity)
getAllEntitiesInBoundingBox mins maxs =
  getEntitiesInBoundingBox' mins maxs (-1)

getEntitiesInBoundingBox ::
  forall e m.
  (MonadIO m, HasDimension e) =>
  Coord ->
  Coord ->
  GmshT m (V.Vector (Entity e))
getEntitiesInBoundingBox mins maxs =
  V.map (\(MkSomeEntity e) -> coerce e)
    <$> getEntitiesInBoundingBox' mins maxs (entityDim @e Proxy)

getEntitiesInBoundingBox' ::
  MonadIO m => Coord -> Coord -> Dimension -> GmshT m (V.Vector SomeEntity)
getEntitiesInBoundingBox' mins maxs dim =
  fmap (V.map toSomeEntity . V.convert) $
    embedLow $
      Low.gmshModelOccGetEntitiesInBoundingBox mins maxs dim

{- |
Get the bounding box (@xmin@, @ymin@, @zmin@), (@xmax@, @ymax@, @zmax@) of
the OpenCASCADE entity.
-}
getBoundingBox ::
  (HasDimension e, MonadIO m) =>
  Entity e ->
  GmshT m (Coord, Coord)
getBoundingBox e =
  embedLow $
    Low.gmshModelOccGetBoundingBox $ toRawEntity e

{- |
Get the center of mass of the OpenCASCADE entity
-}
getCenterOfMass ::
  (HasDimension e, MonadIO m) =>
  Entity e ->
  GmshT m Coord
getCenterOfMass e =
  embedLow $
    Low.gmshModelOccGetCenterOfMass $ toRawEntity e

dilate' ::
  (MonadIO m, Foldable t) =>
  Double ->
  Double ->
  Double ->
  Coord ->
  t SomeEntity ->
  GmshT m ()
dilate' a b c center entities =
  embedLow_ $
    Low.gmshModelOccDilate
      (L.fold (L.premap someToRawEntity L.vector) entities)
      (realToFrac a)
      (realToFrac b)
      (realToFrac c)
      center

dilate ::
  (MonadIO m, HasDimension e, Foldable t) =>
  Double ->
  Double ->
  Double ->
  Coord ->
  t (Entity e) ->
  GmshT m ()
dilate a b c center = dilate' a b c center . map MkSomeEntity . F.toList

rotate' ::
  (Foldable t, MonadIO m) =>
  Coord ->
  Vec3 ->
  Radian ->
  t SomeEntity ->
  GmshT m ()
rotate' center axis rad ents =
  embedLow_ $
    Low.gmshModelOccRotate (L.fold (L.premap someToRawEntity L.vector) ents) center axis (realToFrac rad)

rotate ::
  (HasDimension e, Foldable t, MonadIO m) =>
  Coord ->
  Vec3 ->
  Radian ->
  t (Entity e) ->
  GmshT m ()
rotate center axis rad =
  rotate' center axis rad
    . map MkSomeEntity
    . F.toList

translate' ::
  (Foldable t, MonadIO m) =>
  Vec3 ->
  t SomeEntity ->
  GmshT m ()
translate' move ents =
  embedLow_ $
    Low.gmshModelOccTranslate (L.fold (L.premap someToRawEntity L.vector) ents) move

translate ::
  (HasDimension e, Foldable t, MonadIO m) =>
  Vec3 ->
  t (Entity e) ->
  GmshT m ()
translate move =
  translate' move
    . map MkSomeEntity
    . F.toList

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
    Low.gmshModelOccMirror
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

{- |
Add a parallelepipedic box in the OpenCASCADE CAD representation, defined
by a point (@x@, @y@, @z@) and the extents along the x-, y- and z-axes.
-}
addBox :: (MonadIO m) => Coord -> Vec3 -> GmshT m Volume
addBox coord extent =
  fmap Entity $
    embedLow $ Low.gmshModelOccAddBox coord extent (-1)

{- |
Add a parallelepipedic box in the OpenCASCADE CAD representation, defined
by a point (@x@, @y@, @z@) and the extents along the x-, y- and z-axes. If
@tag@ is positive, set the tag explicitly; otherwise a new tag is selected
automatically. Return the tag of the box.
-}
addBoxAs :: (MonadIO m) => Coord -> Vec3 -> VolumeTag -> GmshT m Volume
addBoxAs coord extent (EntityTag raw) =
  fmap Entity $
    embedLow $ Low.gmshModelOccAddBox coord extent raw

remove' :: (MonadIO m, Foldable t) => Bool -> t SomeEntity -> GmshT m ()
remove' recursive entities =
  embedLow_ $
    Low.gmshModelOccRemove (L.fold (L.premap someToRawEntity L.vector) entities) recursive

remove :: (MonadIO m, Foldable t, HasDimension e) => Bool -> t (Entity e) -> GmshT m ()
remove recursive =
  remove' recursive . map MkSomeEntity . F.toList

addRoundedRectangle ::
  MonadIO m =>
  Coord ->
  Vec2 ->
  Double ->
  GmshT m Surface
addRoundedRectangle cd dxy roundedRadius =
  fmap Entity $ embedLow $ Low.gmshModelOccAddRectangle cd dxy (-1) (realToFrac roundedRadius)

addRectangle ::
  MonadIO m =>
  Coord ->
  Vec2 ->
  GmshT m Surface
addRectangle cd dxy =
  fmap Entity $ embedLow $ Low.gmshModelOccAddRectangle cd dxy (-1) 0.0

{- |
Set a color option to the RGBA value (@r@, @g@, @b@, @a@), where where @r@,
@g@, @b@ and @a@ should be integers between 0 and 255. @name@ is of the
form @"category.option"@ or @"category[num].option"@. Available categories and
options are listed in the Gmsh reference manual, with the @"Color."@ middle
string removed.
-}
addRectangle' ::
  MonadIO m =>
  Coord ->
  Vec2 ->
  SurfaceTag ->
  Double ->
  GmshT m Surface
addRectangle' cd dxy (EntityTag tag) roundedRadius =
  fmap Entity $
    embedLow $
      Low.gmshModelOccAddRectangle cd dxy tag $
        realToFrac roundedRadius

{- |
Add a surface in the OpenCASCADE CAD representation, filling the curve loop
@wireTag@. If @tag@ is positive, set the tag explicitly; otherwise a new
tag is selected automatically. Return the tag of the surface. If
@pointTags@ are provided, force the surface to pass through the given
points.
-}
addSurfaceFilling' ::
  (Foldable t, MonadIO m) =>
  CurveLoop ->
  SurfaceTag ->
  t Point ->
  GmshT m Surface
addSurfaceFilling' cl tag =
  fmap Entity
    . embedLow
    . Low.gmshModelOccAddSurfaceFilling
      (rawEntityTag cl)
      (getRawEntityTag tag)
    . L.fold (L.premap rawEntityTag L.vector)

{- |
Add a surface in the OpenCASCADE CAD representation, filling the curve loop
@wireTag@.
Return the tag of the surface.
If @pointTags@ are provided, force the surface to pass through the given
points.
-}
addSurfaceFilling :: (Foldable t, MonadIO m) => CurveLoop -> t Point -> GmshT m Surface
addSurfaceFilling cl = addSurfaceFilling' cl (-1)
