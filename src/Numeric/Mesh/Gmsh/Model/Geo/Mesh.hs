module Numeric.Mesh.Gmsh.Model.Geo.Mesh
  ( setTransfiniteCurve,
    setTransfiniteSurface,
    setTransfiniteVolume,
    setRecombine,
  )
where

import qualified Control.Foldl as L
import Control.Monad.IO.Class (MonadIO)
import Data.Coerce (coerce)
import Data.Functor.Compose (Compose (Compose))
import Numeric.Mesh.Gmsh
import qualified Numeric.Mesh.Gmsh.LowLevel as Low

{- |
Set a transfinite meshing constraint on the curve @tag@ in the built-in CAD
kernel representation, with @numNodes@ nodes distributed according to
@meshType@ and @coef@. Currently supported types are @"Progression"@
(geometrical progression with power @coef@) and @"Bump"@ (refinement toward
both extremities of the curve)
-}
setTransfiniteCurve ::
  MonadIO m =>
  Curve ->
  -- | Number of nodes
  Int ->
  -- | Transfinite method
  CurveTransfiniteMethod ->
  -- | Coefficient
  Double ->
  GmshT m ()
setTransfiniteCurve curve numNodes method coeff =
  embedLow_ $
    Low.gmshModelGeoMeshSetTransfiniteCurve (coerce curve) (fromIntegral numNodes) (renderTransfiniteCurveMethod method) (realToFrac coeff)

{- |
Set a transfinite meshing constraint on the surface @tag@ in the built-in
CAD kernel representation. @arrangement@ describes the arrangement of the
triangles when the surface is not flagged as recombined: currently
supported values are 'Left', 'Right', 'AlternateLeft' and 'AlternateRight'.
@cornerTags@ can be used to specify the (3 or 4) corners of the transfinite
interpolation explicitly; specifying the corners explicitly is mandatory if
the surface has more that 3 or 4 points on its boundary.
-}
setTransfiniteSurface ::
  (Foldable t, MonadIO m) =>
  Surface ->
  TransSurfaceArrangement ->
  t Point ->
  GmshT m ()
setTransfiniteSurface surf arrang corners =
  embedLow_ $
    Low.gmshModelGeoMeshSetTransfiniteSurface
      (coerce surf)
      (renderTransSurfaceArrangement arrang)
      $ L.fold (L.premap rawEntityTag L.vector) corners

{- |
Set a transfinite meshing constraint on the surface @tag@ in the built-in
CAD kernel representation. @cornerTags@ can be used to specify the (6 or 8)
corners of the transfinite interpolation explicitly.
-}
setTransfiniteVolume ::
  (MonadIO m) =>
  Volume ->
  Maybe (TransVolumeCorners Point) ->
  GmshT m ()
setTransfiniteVolume (Entity vol) points =
  embedLow_ $
    Low.gmshModelGeoMeshSetTransfiniteVolume vol $
      L.fold (L.premap rawEntityTag L.vector) (Compose points)

{- |
Set a recombination meshing constraint on the entity of dimension @dim@ and
tag @tag@ in the built-in CAD kernel representation. Currently only
entities of dimension 2 (to recombine triangles into quadrangles) are
supported.
-}
setRecombine :: (MonadIO m) => Surface -> Double -> GmshT m ()
setRecombine dimTag =
  embedLow_ . Low.gmshModelGeoMeshSetRecombine (toRawEntity dimTag) . realToFrac
