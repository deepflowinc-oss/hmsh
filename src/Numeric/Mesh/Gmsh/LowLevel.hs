{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fobject-code #-}

{- | Almost literal low-level binding to <gmshc.h>.
Some types are replaced with Haskell-friendly types or newtype-wrapped.

Note that eturned vectors are storable ones to reduce further allocations.
However, as a type parameter @a@ in storable @'S.Vector' a@ behaves as /phantom/,
users can easily 'coerce' Storable vectors into non-compatible ones.
Hence, you should wrap such functions with conversion to/from Boxed or Unboxed vectors for high-level implementation.
-}
module Numeric.Mesh.Gmsh.LowLevel
  ( module Numeric.Mesh.Gmsh.LowLevel.Types,
    gmshInitialize,
    gmshFinalize,
    gmshOpen,
    gmshMerge,
    gmshWrite,
    gmshClear,
    gmshOptionSetNumber,
    gmshOptionGetNumber,
    gmshOptionSetString,
    gmshOptionGetString,
    gmshOptionSetColor,
    gmshOptionGetColor,
    gmshModelAdd,
    gmshModelRemove,
    gmshModelList,
    gmshModelGetCurrent,
    gmshModelSetCurrent,
    gmshModelGetFileName,
    gmshModelSetFileName,
    gmshModelGetEntities,
    gmshModelSetEntityName,
    gmshModelGetEntityName,
    gmshModelGetPhysicalGroups,
    gmshModelGetEntitiesForPhysicalGroup,
    gmshModelGetPhysicalGroupsForEntity,
    gmshModelAddPhysicalGroup,
    gmshModelRemovePhysicalGroups,
    gmshModelSetPhysicalName,
    gmshModelRemovePhysicalName,
    gmshModelGetPhysicalName,
    gmshModelGetBoundary,
    gmshModelGetAdjacencies,
    gmshModelGetEntitiesInBoundingBox,
    gmshModelGetBoundingBox,
    gmshModelGetDimension,
    gmshModelGetType,
    gmshModelAddDiscreteEntity,
    gmshModelRemoveEntities,
    gmshModelRemoveEntityName,
    gmshModelGetParent,
    gmshModelGetPartitions,
    gmshModelGetValue,
    gmshModelGetDerivative,
    gmshModelGetSecondDerivative,
    gmshModelGetCurvature,
    gmshModelGetPrincipalCurvatures,
    gmshModelGetNormal,
    gmshModelGetParametrization,
    gmshModelGetParametrizationBounds,
    gmshModelIsInside,
    gmshModelGetClosestPoint,
    gmshModelReparametrizeOnSurface,
    gmshModelSetVisibility,
    gmshModelGetVisibility,
    gmshModelSetVisibilityPerWindow,
    gmshModelSetColor,
    gmshModelGetColor,
    gmshModelSetCoordinates,
    gmshModelMeshGenerate,
    gmshModelMeshPartition,
    gmshModelMeshUnpartition,
    gmshModelMeshOptimize,
    gmshModelMeshRecombine,
    gmshModelMeshRefine,
    gmshModelMeshSetOrder,
    gmshModelMeshGetLastEntityError,
    gmshModelMeshGetLastNodeError,
    gmshModelMeshClear,
    gmshModelMeshGetNodes,
    gmshModelMeshGetElements,
    gmshModelMeshGetElement,
    gmshModelMeshSetTransfiniteCurve,
    gmshModelMeshSetTransfiniteSurface,
    gmshModelMeshSetTransfiniteVolume,
    gmshModelMeshSetTransfiniteAutomatic,
    gmshModelMeshSetRecombine,
    gmshModelMeshRemoveDuplicateNodes,
    gmshModelMeshFieldAdd,
    gmshModelMeshFieldRemove,
    gmshModelMeshFieldSetNumber,
    gmshModelMeshFieldSetString,
    gmshModelMeshFieldSetNumbers,
    gmshModelMeshFieldSetAsBackgroundMesh,
    gmshModelMeshFieldSetAsBoundaryLayer,
    gmshModelGeoSynchronize,
    gmshModelOccSynchronize,
    gmshModelGeoAddPoint,
    gmshModelGeoAddLine,
    gmshModelGeoAddCircleArc,
    gmshModelGeoAddEllipseArc,
    gmshModelGeoAddSpline,
    gmshModelGeoAddBSpline,
    gmshModelGeoAddBezier,
    gmshModelGeoAddPolyline,
    gmshModelGeoAddCompoundSpline,
    gmshModelGeoAddCompoundBSpline,
    gmshModelGeoAddCurveLoop,
    gmshModelGeoAddCurveLoops,
    gmshModelGeoAddPlaneSurface,
    gmshModelGeoAddSurfaceLoop,
    gmshModelGeoAddVolume,
    gmshModelGeoExtrude,
    gmshModelGeoCopy,
    gmshModelGeoRevolve,
    gmshModelGeoMirror,
    gmshModelGeoMeshSetTransfiniteCurve,
    gmshModelGeoMeshSetTransfiniteSurface,
    gmshModelGeoMeshSetTransfiniteVolume,
    gmshModelGeoMeshSetRecombine,
    gmshLoggerGetLastError,
    gmshLoggerStart,
    gmshLoggerStop,
    gmshModelGeoRemoveAllDuplicates,
    gmshModelOccRemoveAllDuplicates,
    gmshModelOccAddPoint,
    gmshModelOccAddLine,
    gmshModelOccAddCircleArc,
    gmshModelOccAddCircle,
    gmshModelOccAddEllipseArc,
    gmshModelOccAddEllipse,
    gmshModelOccAddSpline,
    gmshModelOccAddBezier,
    gmshModelOccAddCurveLoop,
    gmshModelOccAddPlaneSurface,
    gmshModelOccAddSurfaceLoop,
    gmshModelOccAddVolume,
    gmshModelOccAddCylinder,
    gmshModelOccExtrude,
    gmshModelOccRevolve,
    gmshModelOccRotate,
    gmshModelOccIntersect,
    gmshModelOccCut,
    gmshModelOccFuse,
    gmshModelOccFragment,
    gmshModelOccCopy,
    gmshModelOccGetEntitiesInBoundingBox,
    gmshModelOccGetBoundingBox,
    gmshModelOccGetCenterOfMass,
    gmshModelOccDilate,
    gmshModelOccGetEntities,
    gmshModelOccTranslate,
    gmshModelOccMirror,
    gmshModelOccAddBox,
    gmshModelOccRemove,
    gmshModelOccAddRectangle,
    gmshModelOccAddSurfaceFilling,
    gmshFltkInitialize,
    gmshFltkUpdate,
    gmshFltkRun,
    gmshFltkWait,
  )
where

import Control.Arrow ((***))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Coerce (coerce)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import Language.C.Inline hiding (pure)
import qualified Language.C.Inline as C hiding (pure)
import Numeric.Mesh.Gmsh.LowLevel.Types
import Numeric.Mesh.Gmsh.LowLevel.Utils

C.context $ C.baseCtx <> C.bsCtx <> C.vecCtx
C.include "<gmshc.h>"

-- For local use only.
pattern CEntity :: CInt -> CInt -> Entity
pattern CEntity dim tag = Entity (Dimension dim) (EntityTag tag)

{-# COMPLETE CEntity #-}

{- |
Initialize Gmsh API. This must be called before any call to the other
functions in the API. If  @argv@  are provided,
they will be handled in the same way as the command
line arguments in the Gmsh app. If @readConfigFiles@ is set, read system
Gmsh configuration files (gmshrc and gmsh-options). Initializing the API
sets the options @"General.Terminal"@ to @1@ and @"General.AbortOnError"@ to @2@.
-}
gmshInitialize ::
  -- | @argv@
  [BS.ByteString] ->
  -- | @readConfigFile@
  Bool ->
  IO StatusCode
gmshInitialize argv0 (bool -> readConfigFiles) =
  withStatusCode_ $ \ierr -> evalContT $ do
    argv <- S.fromList <$> mapM (ContT . BS8.useAsCString) argv0
    liftIO
      [C.exp|void { 
      gmshInitialize(
          $vec-len:argv,
          $vec-ptr:(char **argv),
          $(const int readConfigFiles),
          $(int *ierr)
      ) }
    |]

{- |
Finalize the Gmsh API. This must be called when you are done using the Gmsh API.
-}
gmshFinalize :: IO StatusCode
gmshFinalize = withStatusCode_ $ \ierr ->
  [C.exp|void { gmshFinalize($(int *ierr)) }|]

{- |
Open a file. Equivalent to the File->Open menu in the Gmsh app. Handling
of the file depends on its extension and/or its contents: opening a file
with model data will create a new model.
-}
gmshOpen ::
  -- | @fileName@
  BS.ByteString ->
  IO StatusCode
gmshOpen fileName = withStatusCode_ $ \ierr ->
  [C.exp|void {gmshOpen($bs-cstr:fileName, $(int *ierr))}|]

{- |
Merge a file. Equivalent to the File->Merge menu in the Gmsh app.
Handling of the file depends on its extension and/or its contents. Merging
a file with model data will add the data to the current model.
-}
gmshMerge ::
  -- | @fileName@
  BS.ByteString ->
  IO StatusCode
gmshMerge fileName = withStatusCode_ $ \ierr ->
  [C.exp|void {gmshMerge($bs-cstr:fileName, $(int *ierr))}|]

-- | Write a file. The export format is determined by the file extension.
gmshWrite ::
  -- | @fileName@
  BS.ByteString ->
  IO StatusCode
gmshWrite fileName = withStatusCode_ $ \ierr ->
  [C.exp|void { gmshWrite($bs-cstr:fileName, $(int *ierr)) }|]

-- | Clear all loaded models and post-processing data, and add a new empty model.
gmshClear :: IO StatusCode
gmshClear = withStatusCode_ $ \ierr ->
  [C.exp|void { gmshClear($(int *ierr)) }|]

{- |
Set a numerical option to @value@. @name@ is of the form @"category.option"@
or @"category[num].option"@. Available categories and options are listed in
the Gmsh reference manual.
-}
gmshOptionSetNumber ::
  -- | @name@
  BS.ByteString ->
  -- | @value@
  CDouble ->
  IO StatusCode
gmshOptionSetNumber name value = withStatusCode_ $ \ierr ->
  [C.exp|void {
    gmshOptionSetNumber($bs-cstr:name,
                        $(const double value),
                        $(int * ierr))
  }|]

{- |
Get the @value@ of a numerical option. @name@ is of the form
@"category.option"@ or @"category[num].option"@. Available categories and
options are listed in the Gmsh reference manual.
-}
gmshOptionGetNumber ::
  -- | @name@
  BS.ByteString ->
  -- | @(value, ierr)@
  IO (CDouble, StatusCode)
gmshOptionGetNumber name = withPtr $ \value -> withStatusCode_ $ \ierr ->
  [C.exp|void {
    gmshOptionGetNumber($bs-cstr:name,
                        $(double * value),
                        $(int * ierr))
  }|]

{- |
Set a string option to @value@. @name@ is of the form @"category.option"@ or
@"category[num].option"@. Available categories and options are listed in the
Gmsh reference manual.
-}
gmshOptionSetString ::
  -- | @name@
  BS.ByteString ->
  -- | @value@
  BS.ByteString ->
  IO StatusCode
gmshOptionSetString name value = withStatusCode_ $ \ierr ->
  [C.exp|void { 
    gmshOptionSetString($bs-cstr:name, $bs-cstr:value, $(int *ierr))
  }|]

{- |
Get the @value@ of a string option. @name@ is of the form @"category.option"@
or @"category[num].option"@. Available categories and options are listed in
the Gmsh reference manual.
-}
gmshOptionGetString ::
  -- | @name@
  BS.ByteString ->
  -- | @(value, ierr)@
  IO (BS.ByteString, StatusCode)
gmshOptionGetString name = withStatusCode $ \ierr -> do
  cstr <- withPtr_ $ \value ->
    [C.exp|void {
      gmshOptionGetString($bs-cstr:name,
                          $(char **value),
                          $(int *ierr))
      }|]
  BS8.packCString cstr

{- |
Set a color option to the RGBA value (@r@, @g@, @b@, @a@), where where @r@,
@g@, @b@ and @a@ should be integers between 0 and 255. @name@ is of the
form @"category.option"@ or @"category[num].option"@. Available categories and
options are listed in the Gmsh reference manual, with the @"Color."@ middle
string removed.
-}
gmshOptionSetColor ::
  -- | @name@
  BS.ByteString ->
  -- | @r@
  RGBA ->
  IO StatusCode
gmshOptionSetColor name RGBA {..} = withStatusCode_ $ \ierr ->
  [C.exp|void {
    gmshOptionSetColor(
        $bs-cstr:name,
        $(const int red),
        $(const int green),
        $(const int blue),
        $(const int alpha),
        $(int * ierr)) 
    } |]

{- |
Get the @r@, @g@, @b@, @a@ value of a color option. @name@ is of the form
@"category.option"@ or @"category[num].option"@. Available categories and
options are listed in the Gmsh reference manual, with the @"Color."@ middle
string removed.
-}
gmshOptionGetColor ::
  -- | @name@
  BS.ByteString ->
  -- | @((r,g,b,a), ierr)@
  IO (RGBA, StatusCode)
gmshOptionGetColor name = withStatusCode $ \ierr ->
  withPtrs_ $ \(r, g, b, a) ->
    [C.exp| void {
      gmshOptionGetColor($bs-cstr:name,
                         $(int * r),
                        $(int * g),
                        $(int * b),
                        $(int * a),
                        $(int * ierr))
    }|]

-- | Add a new model, with name @name@, and set it as the current model.
gmshModelAdd ::
  -- | @name@
  BS8.ByteString ->
  IO StatusCode
gmshModelAdd name = withStatusCode_ $ \ierr ->
  [C.exp|void {gmshModelAdd($bs-cstr:name, $(int * ierr))}|]

-- | Remove the current model.
gmshModelRemove :: IO StatusCode
gmshModelRemove = withStatusCode_ $ \ierr ->
  [C.exp|void {gmshModelRemove($(int * ierr))} |]

-- | List the names of all models.
gmshModelList :: IO (V.Vector BS8.ByteString, StatusCode)
gmshModelList = withStatusCode $ \ierr ->
  withCStrings_ $ \names len ->
    [C.exp|void { gmshModelList($(char *** names), $(size_t * len),
                            $(int * ierr))

      }|]

-- | Get the name of the current model.
gmshModelGetCurrent :: IO (BS8.ByteString, StatusCode)
gmshModelGetCurrent = withStatusCode $ \ierr -> withCString_ $ \name ->
  [C.exp|void {
    gmshModelGetCurrent($(char **name), $(int * ierr))
  }|]

{- |
Set the current model to the model with name @name@. If several models have
the same name, select the one that was added first.
-}
gmshModelSetCurrent :: BS8.ByteString -> IO StatusCode
gmshModelSetCurrent name = withStatusCode_ $ \ierr ->
  [C.exp|
  void {
      gmshModelSetCurrent($bs-cstr:name, $(int * ierr))
  }|]

{- |
Get the file name (if any) associated with the current model. A file name
is associated when a model is read from a file on disk.
-}
gmshModelGetFileName :: IO (BS8.ByteString, StatusCode)
gmshModelGetFileName = withStatusCode $ \ierr -> withCString_ $ \fileName ->
  [C.exp|void {
    gmshModelGetFileName($(char ** fileName), $(int * ierr))
  } |]

-- | Set the file name associated with the current model.
gmshModelSetFileName :: BS8.ByteString -> IO StatusCode
gmshModelSetFileName fileName = withStatusCode_ $ \ierr ->
  [C.exp|void {
    gmshModelSetFileName($bs-cstr:fileName, $(int * ierr))
  }|]

{- |
Get all the entities in the current model. If @dim@ is >= 0, return only
the entities of the specified dimension (e.g. points if @dim@ == 0). The
entities are returned as a vector of (dim, tag) integer pairs.
-}
gmshModelGetEntities :: Dimension -> IO (S.Vector Entity, StatusCode)
gmshModelGetEntities (Dimension dim) =
  withStatusCode $ \ierr -> parseEntitiesM $
    withCVector_ $ \dimTags len ->
      [C.exp| void {
        gmshModelGetEntities(
            $(int ** dimTags), $(size_t * len),
              $(const int dim), $(int * ierr))
      }|]

-- | Set the name of the entity.
gmshModelSetEntityName ::
  Entity ->
  EntityName ->
  IO StatusCode
gmshModelSetEntityName (CEntity dim tag) (EntityName name) =
  withStatusCode_ $ \ierr ->
    [C.exp| void {
        gmshModelSetEntityName(
            $(const int dim), $(const int tag), $bs-cstr:name,
            $(int * ierr))
  }|]

-- | Get the name of the entity.
gmshModelGetEntityName :: Entity -> IO (EntityName, StatusCode)
gmshModelGetEntityName (CEntity dim tag) =
  withStatusCode $ \ierr -> coerce $
    withCString_ $ \name ->
      [C.exp| void {
        gmshModelGetEntityName(
            $(const int dim), $(const int tag),
            $(char ** name),
            $(int * ierr)
        )
      }|]

{- | Get all the physical groups in the current model. If @dim@ is >= 0, return
only the entities of the specified dimension (e.g. physical points if @dim@
== 0).
-}
gmshModelGetPhysicalGroups ::
  Dimension -> IO (S.Vector PhysicalGroup, StatusCode)
gmshModelGetPhysicalGroups (Dimension dim) =
  withStatusCode $ \ierr -> parsePhysicalGroupsM $
    withCVector_ $ \dimTags dimTags_n ->
      [C.exp|void {
      gmshModelGetPhysicalGroups(
        $(int ** dimTags), $(size_t * dimTags_n),
        $(const int dim),  $(int * ierr))
    }|]

{- |
Get the tags of the model entities making up the physical group.
-}
gmshModelGetEntitiesForPhysicalGroup ::
  PhysicalGroup -> IO (S.Vector EntityTag, StatusCode)
gmshModelGetEntitiesForPhysicalGroup
  (PhysicalGroup (Dimension dim) (PhysicalTag tag)) =
    withStatusCode $ \ierr -> coerce $
      withCVector_ $ \tags tags_n ->
        [C.exp| void {
          gmshModelGetEntitiesForPhysicalGroup(
          $(const int dim), $(const int tag),
          $(int ** tags), $(size_t * tags_n), $(int * ierr))
      }|]

{- |
Get the tags of the physical groups (if any) to which the model entity belongs.
-}
gmshModelGetPhysicalGroupsForEntity ::
  Entity -> IO (S.Vector PhysicalTag, StatusCode)
gmshModelGetPhysicalGroupsForEntity (CEntity dim tag) =
  withStatusCode $ \ierr -> coerceSVectorM $
    withCVector_ $ \physicalTags physicalTags_n ->
      [C.exp|void {
          gmshModelGetPhysicalGroupsForEntity(
            $(const int dim),
            $(const int tag),
            $(int ** physicalTags), $(size_t * physicalTags_n),
            $(int * ierr)
          )
        }|]

{- |
Add a physical group of dimension @dim@, grouping the model entities with
tags @entityTags@. Return the tag of the physical group, equal to @physTag@ if @physTag@
is positive, or a new tag if @physTag@ < 0.
-}
gmshModelAddPhysicalGroup ::
  Dimension ->
  S.Vector EntityTag ->
  PhysicalTag ->
  IO (PhysicalTag, StatusCode)
gmshModelAddPhysicalGroup
  (Dimension dim)
  (S.unsafeCast -> tags)
  (PhysicalTag tag) = coerce $
    withStatusCode $ \ierr ->
      [C.exp| int {
      gmshModelAddPhysicalGroup(
        $(const int dim)
      , $vec-ptr:(int *tags)
      , $vec-len:tags
      , $(const int tag)
      , $(int * ierr)
      )
  }|]

{-
Remove the physical groups @physGroups@ from the current model. If @physGroups@
is empty, remove all groups.
-}
gmshModelRemovePhysicalGroups :: S.Vector Entity -> IO StatusCode
gmshModelRemovePhysicalGroups (encodeEntities -> entities) =
  withStatusCode_ $ \ierr ->
    [C.exp| void {
      gmshModelRemovePhysicalGroups(
        $vec-ptr:(int *entities)
      , $vec-len:entities
      , $(int * ierr)
      )
    }|]

-- | Set the name of the physical group.
gmshModelSetPhysicalName :: PhysicalGroup -> PhysicalName -> IO StatusCode
gmshModelSetPhysicalName
  (PhysicalGroup (Dimension dim) (PhysicalTag tag))
  (PhysicalName name) =
    withStatusCode_ $ \ierr ->
      [C.exp| void {
      gmshModelSetPhysicalName(
        $(const int dim),
        $(const int tag),
        $bs-cstr:name,
        $(int * ierr))
  }|]

-- | Remove the physical name @name@ from the current model.
gmshModelRemovePhysicalName :: PhysicalName -> IO StatusCode
gmshModelRemovePhysicalName (PhysicalName name) = withStatusCode_ $ \ierr ->
  [C.exp| void {
    gmshModelRemovePhysicalName
      ( $bs-cstr:name
      , $(int * ierr)
      )
  }|]

-- | Get the name of the physical group.
gmshModelGetPhysicalName :: PhysicalGroup -> IO (PhysicalName, StatusCode)
gmshModelGetPhysicalName
  (PhysicalGroup (Dimension dim) (PhysicalTag tag)) =
    withStatusCode $ \ierr -> coerce @_ @(IO PhysicalName) $
      withCString_ $ \name ->
        [C.exp| void {
            gmshModelGetPhysicalName
              ( $(const int dim)
              , $(const int tag)
              , $(char **name)
              , $(int * ierr)
              )
      }|]

{- |
Get the boundary of the model entities @dimTags@. Return
the boundary of the individual entities (if @combined@ is false) or the
boundary of the combined geometrical shape formed by all input entities (if
@combined@ is true). Return tags multiplied by the sign of the boundary
entity if @oriented@ is true. Apply the boundary operator recursively down
to dimension 0 (i.e. to points) if @recursive@ is true.
-}
gmshModelGetBoundary ::
  S.Vector Entity ->
  -- | @combined@
  Bool ->
  -- | @oriented@
  Bool ->
  -- | @recursive@
  Bool ->
  IO (S.Vector Entity, StatusCode)
gmshModelGetBoundary
  (encodeEntities -> dimTags)
  (bool -> combined)
  (bool -> oriented)
  (bool -> recursive) =
    withStatusCode $ \ierr -> parseEntitiesM $
      withCVector_ $ \outDimTags outDimTags_n ->
        [C.exp| void {
          gmshModelGetBoundary
            ( $vec-ptr:(int *dimTags)
            , $vec-len:dimTags
            , $(int ** outDimTags)
            , $(size_t * outDimTags_n)
            , $(const int combined)
            , $(const int oriented)
            , $(const int recursive)
            , $(int * ierr)
            )
        }|]

{- |

Get the upward and downward adjacencies of the model entity.
The @upward@ vector returns the adjacent entities of
dimension @dim@ + 1; the @downward@ vector returns the adjacent entities of
dimension @dim@ - 1.
-}
gmshModelGetAdjacencies :: Entity -> IO (Adjacency, StatusCode)
gmshModelGetAdjacencies (CEntity dim tag) =
  withStatusCode $ \ierr -> do
    (upwards, downwards) <- withCVector $ \upward upward_n ->
      withCVector_ $ \downward downward_n ->
        [C.exp| void {
          gmshModelGetAdjacencies
            ( $(const int dim)
            , $(const int tag)
            , $(int ** upward)
            , $(size_t * upward_n)
            , $(int ** downward)
            , $(size_t * downward_n)
            , $(int * ierr)
            )
          }|]
    pure
      Adjacency
        { upward = coerce upwards
        , downward = coerce downwards
        }

{- |
Get the model entities in the bounding box defined by the two points
@mins@ and @maxs@. If @dim@ is >= 0,
return only the entities of the specified dimension (e.g. points if @dim@ == 0).
-}
gmshModelGetEntitiesInBoundingBox ::
  -- | @mins@
  Coord ->
  -- | @maxs@
  Coord ->
  Dimension ->
  IO (S.Vector Entity, StatusCode)
gmshModelGetEntitiesInBoundingBox
  (Coord (Vec3 xmin ymin zmin))
  (Coord (Vec3 xmax ymax zmax))
  (Dimension dim) =
    withStatusCode $ \ierr -> parseEntitiesM $
      withCVector_ $ \tags tags_n ->
        [C.exp|void {
          gmshModelGetEntitiesInBoundingBox
          ( $(const double xmin)
          , $(const double ymin)
          , $(const double zmin)
          , $(const double xmax)
          , $(const double ymax)
          , $(const double zmax)
          , $(int ** tags)
          , $(size_t * tags_n)
          , $(const int dim)
          , $(int * ierr)
          )
        }|]

{- |
Get the bounding box (@xmin@, @ymin@, @zmin@), (@xmax@, @ymax@, @zmax@) of
the model entity. If @dim@ and @tag@ are
negative, get the bounding box of the whole model.
-}
gmshModelGetBoundingBox ::
  Entity ->
  -- | @((mins, maxs), ierr)@
  IO ((Coord, Coord), StatusCode)
gmshModelGetBoundingBox (CEntity dim tag) =
  withStatusCode $ \ierr -> withPtrs $ \(xmin, ymin, zmin) ->
    withPtrs_ $ \(xmax, ymax, zmax) ->
      [C.exp|void { 
        gmshModelGetBoundingBox(
          $(const int dim), $(const int tag)
        , $(double * xmin), $(double * ymin), $(double * zmin)
        , $(double * xmax), $(double * ymax), $(double * zmax)
        , $(int * ierr)
        )
      }|]

-- | Get the geometrical dimension of the current model.
gmshModelGetDimension ::
  IO (Dimension, StatusCode)
gmshModelGetDimension = withStatusCode $ \ierr ->
  Dimension
    <$> [C.exp| int {gmshModelGetDimension($(int * ierr))} |]

{- |
Add a discrete model entity (defined by a mesh) of dimension @dim@ in the
current model. Return the tag of the new discrete entity, equal to @tag@ if
@tag@ is positive, or a new tag if @tag@ < 0. @boundary@ specifies the tags
of the entities on the boundary of the discrete entity, if any. Specifying
@boundary@ allows Gmsh to construct the topology of the overall model.
-}
gmshModelAddDiscreteEntity ::
  Dimension ->
  -- | @tag@
  EntityTag ->
  -- | @boundary@
  S.Vector EntityTag ->
  IO (EntityTag, StatusCode)
gmshModelAddDiscreteEntity
  (Dimension dim)
  (EntityTag tag)
  (S.unsafeCast @EntityTag @CInt -> boundary) =
    withStatusCode $ \ierr ->
      EntityTag
        <$> [C.exp| int {
              gmshModelAddDiscreteEntity
              ( $(const int dim)
              , $(const int tag)
              , $vec-ptr:(int * boundary)
              , $vec-len:boundary
              , $(int * ierr)
              )
            }|]

{- |
Remove the entities of the current model. If @recursive@ is true,
remove all the entities on their boundaries, down to dimension 0.
-}
gmshModelRemoveEntities :: S.Vector Entity -> Bool -> IO StatusCode
gmshModelRemoveEntities
  (encodeEntities -> dimTags)
  (bool -> recursive) =
    withStatusCode_ $ \ierr ->
      [C.exp|void { 
        gmshModelRemoveEntities
          ( $vec-ptr:(int *dimTags)
          , $vec-len:dimTags
          , $(const int recursive)
          , $(int * ierr)
          )
    }|]

-- | Remove the entity name @name@ from the current model.
gmshModelRemoveEntityName :: EntityName -> IO StatusCode
gmshModelRemoveEntityName (EntityName name) =
  withStatusCode_ $ \ierr ->
    [C.exp| void {gmshModelRemoveEntityName($bs-cstr:name, $(int * ierr))} |]

-- | Get the type of the entity.
gmshModelGetType :: Entity -> IO (EntityType, StatusCode)
gmshModelGetType (CEntity dim tag) =
  withStatusCode $ \ierr -> fmap EntityType $
    withCString_ $ \entityType ->
      [C.exp| void {
      gmshModelGetType
        ( $(const int dim)
        , $(const int tag)
        , $(char **entityType)
        , $(int * ierr)
        )
    }|]

{- |
In a partitioned model, get the parent of the entity,
i.e. from which the entity is a part of, if any. @parentDim@ and
@parentTag@ are set to -1 if the entity has no parent.
-}
gmshModelGetParent :: Entity -> IO (Entity, StatusCode)
gmshModelGetParent (CEntity dim tag) =
  withStatusCode $ \ierr ->
    withPtrs_ $ \(parentDim, parentTag) ->
      [C.exp| void {
      gmshModelGetParent(
        $(const int dim)
      , $(const int tag)
      , $(int * parentDim)
      , $(int * parentTag)
      , $(int * ierr)
  )}|]

{- |
In a partitioned model, return the tags of the partition(s) to which the
entity belongs.
-}
gmshModelGetPartitions :: Entity -> IO (S.Vector EntityTag, StatusCode)
gmshModelGetPartitions (CEntity dim tag) =
  withStatusCode $ \ierr -> coerceSVectorM @EntityTag $
    withCVector_ $ \partitions partitions_n ->
      [C.exp|void {
        gmshModelGetPartitions
        ( $(const int dim)
        , $(const int tag)
        , $(int ** partitions)
        , $(size_t * partitions_n)
        , $(int * ierr)
    )}|]

{- |
Evaluate the parametrization of the entity
at the parametric coordinates @parametricCoord@. Only valid for @dim@ equal
to 0 (with empty @parametricCoord@), 1 (with @parametricCoord@ containing
parametric coordinates on the curve) or 2 (with @parametricCoord@
containing pairs of u, v parametric coordinates on the surface,
concatenated: [p1u, p1v, p2u, ...]).
-}
gmshModelGetValue ::
  Entity ->
  -- | @parametricCoord@
  S.Vector CDouble ->
  IO (S.Vector Coord, StatusCode)
gmshModelGetValue (CEntity dim tag) parametricCoord =
  withStatusCode $ \ierr ->
    parseCoordsM $
      withCVector_ $ \coord coord_n ->
        [C.exp|void {
            gmshModelGetValue
              ( $(const int dim)
              , $(const int tag)
              , $vec-ptr:(double * parametricCoord)
              , $vec-len:parametricCoord
              , $(double ** coord)
              , $(size_t * coord_n)
              , $(int * ierr)
              )
          }|]

{- |
Evaluate the derivative of the parametrization of the entity
at the parametric coordinates @parametricCoord@. Only
valid for @dim@ equal to 1 (with @parametricCoord@ containing parametric
coordinates on the curve) or 2 (with @parametricCoord@ containing pairs of
u, v parametric coordinates on the surface, concatenated: [p1u, p1v, p2u,
...]). For @dim@ equal to 1 return the x, y, z components of the derivative
with respect to u [d1ux, d1uy, d1uz, d2ux, ...]; for @dim@ equal to 2
return the x, y, z components of the derivative with respect to u and v:
[d1ux, d1uy, d1uz, d1vx, d1vy, d1vz, d2ux, ...].
-}
gmshModelGetDerivative ::
  Entity ->
  -- | @parametricCoord@
  S.Vector CDouble ->
  IO (S.Vector Coord, StatusCode)
gmshModelGetDerivative (CEntity dim tag) parametricCoord =
  withStatusCode $ \ierr -> parseCoordsM $
    withCVector_ $ \derivatives derivatives_n ->
      [C.exp| void {
          gmshModelGetDerivative
           ( $(const int dim)
           , $(const int tag)
           , $vec-ptr:(double * parametricCoord)
           , $vec-len:parametricCoord
           , $(double ** derivatives)
           , $(size_t * derivatives_n)
           , $(int * ierr)
           ) } |]

{- |
Evaluate the second derivative of the parametrization of the entity
at the parametric coordinates@parametricCoord@.
Only valid for @dim@ equal to 1 (with @parametricCoord@
containing parametric coordinates on the curve) or 2 (with
@parametricCoord@ containing pairs of u, v parametric coordinates on the
surface, concatenated: [p1u, p1v, p2u, ...]). For @dim@ equal to 1 return
the x, y, z components of the second derivative with respect to u [d1uux,
d1uuy, d1uuz, d2uux, ...]; for @dim@ equal to 2 return the x, y, z
components of the second derivative with respect to u and v, and the mixed
derivative with respect to u and v: [d1uux, d1uuy, d1uuz, d1vvx, d1vvy,
d1vvz, d1uvx, d1uvy, d1uvz, d2uux, ...].
-}
gmshModelGetSecondDerivative ::
  Entity ->
  -- | @parametricCoord@
  S.Vector CDouble ->
  IO (S.Vector Coord, StatusCode)
gmshModelGetSecondDerivative (CEntity dim tag) parametricCoord =
  withStatusCode $ \ierr -> parseCoordsM $
    withCVector_ $ \derivatives derivatives_n ->
      [C.exp| void {
          gmshModelGetSecondDerivative
           ( $(const int dim)
           , $(const int tag)
           , $vec-ptr:(double * parametricCoord)
           , $vec-len:parametricCoord
           , $(double ** derivatives)
           , $(size_t * derivatives_n)
           , $(int * ierr)
          ) }|]

{- |
Evaluate the (maximum) curvature of the entity at the parametric coordinates
@parametricCoord@. Only valid for @dim@
equal to 1 (with @parametricCoord@ containing parametric coordinates on the
curve) or 2 (with @parametricCoord@ containing pairs of u, v parametric
coordinates on the surface, concatenated: [p1u, p1v, p2u, ...]).
-}
gmshModelGetCurvature ::
  Entity ->
  -- | @parametricCoord@
  S.Vector CDouble ->
  IO (S.Vector CDouble, StatusCode)
gmshModelGetCurvature (CEntity dim tag) parametricCoord =
  withStatusCode $ \ierr ->
    withCVector_ $ \curvatures curvatures_n ->
      [C.exp| void {
          gmshModelGetCurvature
           ( $(const int dim)
           , $(const int tag)
           , $vec-ptr:(double * parametricCoord)
           , $vec-len:parametricCoord
           , $(double ** curvatures)
           , $(size_t * curvatures_n)
           , $(int * ierr)
          ) }|]

{- |
Evaluate the principal curvatures of the surface with tag @tag@ at the
parametric coordinates @parametricCoord@, as well as their respective
directions. @parametricCoord@ are given by pair of u and v coordinates.
-}
gmshModelGetPrincipalCurvatures ::
  EntityTag ->
  -- | @parametricCoord@
  S.Vector Vec2 ->
  IO
    ( ( S.Vector CDouble
      , S.Vector CDouble
      , S.Vector Coord
      , S.Vector Coord
      )
    , StatusCode
    )
gmshModelGetPrincipalCurvatures (EntityTag tag) (encodeVec2s -> parametricCoord) = do
  ((cMax, (cMin, (dMax, dMin))), code) <-
    withStatusCode $ \ierr ->
      withCVector $ \curvatureMax curvatureMax_n ->
        withCVector $ \curvatureMin curvatureMin_n ->
          withCVector $ \directionMax directionMax_n ->
            withCVector_ $ \directionMin directionMin_n ->
              [C.exp| void {
                gmshModelGetPrincipalCurvatures
                ( $(const int tag)
                , $vec-ptr:(double * parametricCoord)
                , $vec-len:parametricCoord
                , $(double ** curvatureMax)
                , $(size_t * curvatureMax_n)
                , $(double ** curvatureMin)
                , $(size_t * curvatureMin_n)
                , $(double ** directionMax)
                , $(size_t * directionMax_n)
                , $(double ** directionMin)
                , $(size_t * directionMin_n)
                , $(int * ierr)
          ) }|]
  pure ((cMax, cMin, parseCoords dMax, parseCoords dMin), code)

{- |
Get the normal to the surface with tag @tag@ at the parametric coordinates
@parametricCoord@. @parametricCoord@ are given by pairs of u and v
coordinates.
-}
gmshModelGetNormal ::
  EntityTag ->
  -- | @parametricCoord@
  S.Vector Vec2 ->
  IO (S.Vector Vec3, StatusCode)
gmshModelGetNormal (EntityTag tag) (encodeVec2s -> parametricCoord) =
  withStatusCode $ \ierr -> parseVec3sM $
    withCVector_ $ \normals normals_n ->
      [C.exp| void {
          gmshModelGetNormal
           ( $(const int tag)
           , $vec-ptr:(double * parametricCoord)
           , $vec-len:parametricCoord
           , $(double ** normals)
           , $(size_t * normals_n)
           , $(int * ierr)
          ) }|]

{- |
Get the parametric coordinates @parametricCoord@ for the points @coord@ on
the entity. @coord@ are given as triplets of x, y, z coordinates.
@parametricCoord@ returns the parametric coordinates t on the curve (if
@dim@ = 1) or pairs of u and v coordinates concatenated on the surface (if
@dim@ = 2), i.e. [p1t, p2t, ...] or [p1u, p1v, p2u, ...].
-}
gmshModelGetParametrization ::
  Entity ->
  -- | @coord@
  S.Vector Coord ->
  -- | @(parametricCoord, ierr)@
  IO (S.Vector CDouble, StatusCode)
gmshModelGetParametrization (CEntity dim tag) (encodeCoords -> coord) =
  withStatusCode $ \ierr ->
    withCVector_ $ \parametricCoord parametricCoord_n ->
      [C.exp|void {
        gmshModelGetParametrization
          ( $(const int dim)
          , $(const int tag)
          , $vec-ptr:(double * coord)
          , $vec-len:coord
          , $(double ** parametricCoord)
          , $(size_t * parametricCoord_n)
          , $(int * ierr)
          )
    }|]

-- | Get the @min@ and @max@ bounds of the parametric coordinates for the entity.
gmshModelGetParametrizationBounds ::
  Entity ->
  -- | @(min, max)@
  IO ((S.Vector CDouble, S.Vector CDouble), StatusCode)
gmshModelGetParametrizationBounds (CEntity dim tag) =
  withStatusCode $ \ierr ->
    withCVector $ \mins mins_n ->
      withCVector_ $ \maxs maxs_n ->
        [C.exp|void {
          gmshModelGetParametrizationBounds
            ( $(const int dim), $(const int tag)
            , $(double ** mins), $(size_t * mins_n)
            , $(double ** maxs), $(size_t * maxs_n)
            , $(int * ierr)
            )
        }|]

{- |
Check if the parametric coordinates provided in @parametricCoord@
correspond to points inside the entitiy,
and return the number of points inside. This feature is only available for
a subset of curves and surfaces, depending on the underyling geometrical
representation.
-}
gmshModelIsInside :: Entity -> S.Vector CDouble -> IO (CInt, StatusCode)
gmshModelIsInside (CEntity dim tag) parametricCoord =
  withStatusCode $ \ierr ->
    [C.exp|int  {
      gmshModelIsInside(
        $(const int dim), $(const int tag),
        $vec-ptr:(double * parametricCoord),
        $vec-len:parametricCoord,
        $(int * ierr))
    }|]

{- |
Get the points @closestCoord@ on the entity to the points @coord@,
by orthogonal projection. @coord@ and
@closestCoord@ are given as triplets of x, y, z.
@parametricCoord@ returns the parametric
coordinates t on the curve (if @dim@ = 1) or pairs of u and v coordinates
concatenated on the surface (if @dim@ = 2), i.e. [p1t, p2t, ...] or [p1u,
p1v, p2u, ...].
-}
gmshModelGetClosestPoint ::
  Entity ->
  S.Vector Coord ->
  -- | (@closestCoord@, @parametricCoord@)
  IO ((S.Vector Coord, S.Vector CDouble), StatusCode)
gmshModelGetClosestPoint (CEntity dim tag) (encodeCoords -> coord) =
  withStatusCode $ \ierr -> do
    (cl, p) <- withCVector $ \closestCoord closestCoord_n ->
      withCVector_ $ \parametricCoord parametricCoord_n ->
        [C.exp|void{
      gmshModelGetClosestPoint
        ( $(const int dim)
        , $(const int tag)
        , $vec-ptr:(double * coord)
        , $vec-len:coord
        , $(double ** closestCoord)
        , $(size_t * closestCoord_n)
        , $(double ** parametricCoord)
        , $(size_t * parametricCoord_n)
        , $(int * ierr) 
        )}|]
    pure (parseCoords cl, p)

{- |
Reparametrize the boundary entity (point or curve, i.e. with @dim@ == 0 or
@dim@ == 1) of the surface @surfaceTag@. If @dim@ == 1,
reparametrize all the points corresponding to the parametric coordinates
@parametricCoord@. Multiple matches in case of periodic surfaces can be
selected with @which@. This feature is only available for a subset of
entities, depending on the underyling geometrical representation.
-}
gmshModelReparametrizeOnSurface ::
  Entity ->
  -- | @parametricCoord@
  S.Vector CDouble ->
  -- | @surfaceTag@
  EntityTag ->
  -- | @which@
  CInt ->
  IO (S.Vector CDouble, StatusCode)
gmshModelReparametrizeOnSurface
  (CEntity dim tag)
  parametricCoord
  (EntityTag surfaceTag)
  which =
    withStatusCode $ \ierr ->
      withCVector_ $ \surfaceParametricCoord surfaceParametricCoord_n ->
        [C.exp|void {
          gmshModelReparametrizeOnSurface(
            $(const int dim),
            $(const int tag),
            $vec-ptr:(double * parametricCoord), $vec-len:parametricCoord,
            $(const int surfaceTag),
            $(double ** surfaceParametricCoord),
            $(size_t * surfaceParametricCoord_n),
            $(const int which),
            $(int * ierr))
        }|]

{- |
Set the visibility of the model entities to @value@. Apply the
visibility setting recursively if @recursive@ is true.
-}
gmshModelSetVisibility ::
  S.Vector Entity ->
  -- | @value@
  CInt ->
  -- | @recursive@
  Bool ->
  IO StatusCode
gmshModelSetVisibility (encodeEntities -> dimTags) value (bool -> recursive) =
  withStatusCode_ $ \ierr ->
    [C.exp| void {
    gmshModelSetVisibility
    ( $vec-ptr:(int * dimTags)
    , $vec-len:dimTags
    , $(const int value)
    , $(const int recursive) 
    , $(int * ierr)
    )
  }|]

-- | Get the visibility of the model entity.
gmshModelGetVisibility :: Entity -> IO (CInt, StatusCode)
gmshModelGetVisibility (CEntity dim tag) =
  withStatusCode $ \ierr -> withPtr_ $ \value ->
    [C.exp| void {
        gmshModelGetVisibility
          ( $(const int dim)
          , $(const int tag)
          , $(int * value)
          , $(int * ierr)
          )
    }|]

{- | Set the global visibility of the model per window to @value@, where
@windowIndex@ identifies the window in the window list.
-}
gmshModelSetVisibilityPerWindow ::
  -- | @value@
  CInt ->
  -- | @windowIndex@
  CInt ->
  IO StatusCode
gmshModelSetVisibilityPerWindow value windowIndex =
  withStatusCode_ $ \ierr ->
    [C.exp|void {
      gmshModelSetVisibilityPerWindow(
        $(const int value),
        $(const int windowIndex),
        $(int * ierr))}
    |]

{- |
Set the color of the model entities @dimTags@ to the RGBA value (@r@, @g@,
@b@, @a@), where @r@, @g@, @b@ and @a@ should be integers between 0 and
255. Apply the color setting recursively if @recursive@ is true.
-}
gmshModelSetColor :: S.Vector Entity -> RGBA -> Bool -> IO StatusCode
gmshModelSetColor
  (encodeEntities -> dimTags)
  (RGBA r g b a)
  (bool -> recursive) = withStatusCode_ $ \ierr ->
    [C.exp| void{
      gmshModelSetColor
        ( $vec-ptr:(int * dimTags)
        , $vec-len:dimTags
        , $(const int r)
        , $(const int g)
        , $(const int b)
        , $(const int a)
        , $(const int recursive)
        , $(int * ierr)
        )
    }|]

-- | Get the color of the model entity.
gmshModelGetColor :: Entity -> IO (RGBA, StatusCode)
gmshModelGetColor (CEntity dim tag) =
  withStatusCode $ \ierr ->
    withPtrs_ $ \(r, g, b, a) ->
      [C.exp|void {
        gmshModelGetColor(
          $(const int dim),
          $(const int tag),
          $(int * r),
          $(int * g),
          $(int * b),
          $(int * a),
          $(int * ierr))
      }|]

-- | Set the @x@, @y@, @z@ coordinates of a geometrical point.
gmshModelSetCoordinates :: EntityTag -> Coord -> IO StatusCode
gmshModelSetCoordinates (EntityTag tag) (Coord (Vec3 x y z)) =
  withStatusCode_ $ \ierr ->
    [C.exp| void {gmshModelSetCoordinates(
      $(const int tag)
    , $(const double x), $(const double y), $(const double z)
    , $(int * ierr))
  } |]

-- | Generate a mesh of the current model, up to dimension @dim@ (0, 1, 2 or 3).
gmshModelMeshGenerate :: Dimension -> IO StatusCode
gmshModelMeshGenerate (Dimension dim) =
  withStatusCode_ $ \ierr ->
    [C.exp|void {
      gmshModelMeshGenerate($(const int dim), $(int * ierr));
      }|]

-- | Partition the mesh of the current model into @numPart@ partitions.
gmshModelMeshPartition :: CInt -> IO StatusCode
gmshModelMeshPartition numPart =
  withStatusCode_ $ \ierr ->
    [C.exp|void {
      gmshModelMeshGenerate($(const int numPart), $(int * ierr));
    }|]

-- | Unpartition the mesh of the current model.
gmshModelMeshUnpartition :: IO StatusCode
gmshModelMeshUnpartition = withStatusCode_ $ \ierr ->
  [C.exp|void{gmshModelMeshUnpartition($(int * ierr))}|]

{- | Optimize the mesh of the current model using @method@ (empty for default
tetrahedral mesh optimizer, @"Netgen"@ for Netgen optimizer, @"HighOrder"@ for
direct high-order mesh optimizer, @"HighOrderElastic"@ for high-order elastic
smoother, @"HighOrderFastCurving"@ for fast curving algorithm, @"Laplace2D"@
for Laplace smoothing, @"Relocate2D"@ and @"Relocate3D"@ for node relocation).
If @force@ is set apply the optimization also to discrete entities. If
@entities@ is given, only apply the optimizer to the given entities.
-}
gmshModelMeshOptimize ::
  -- | @method@
  BS8.ByteString ->
  -- | @force@
  Bool ->
  -- | @niter@
  CInt ->
  -- | @entities@
  S.Vector Entity ->
  IO StatusCode
gmshModelMeshOptimize
  method
  (bool -> force)
  niter
  (encodeEntities -> dimTags) =
    withStatusCode_ $ \ierr ->
      [C.exp| void {
      gmshModelMeshOptimize
        ( $bs-cstr:method
        , $(const int force)
        , $(const int niter)
        , $vec-ptr:(int * dimTags)
        , $vec-len:dimTags
        , $(int * ierr)
        )
    }|]

-- | Recombine the mesh of the current model.
gmshModelMeshRecombine :: IO StatusCode
gmshModelMeshRecombine = withStatusCode_ $ \ierr ->
  [C.exp|void{gmshModelMeshRecombine($(int * ierr))}|]

-- | Refine the mesh of the current model by uniformly splitting the elements.
gmshModelMeshRefine :: IO StatusCode
gmshModelMeshRefine = withStatusCode_ $ \ierr ->
  [C.exp|void{gmshModelMeshRefine($(int * ierr))}|]

-- | Set the order of the elements in the mesh of the current model to @order@.
gmshModelMeshSetOrder :: CInt -> IO StatusCode
gmshModelMeshSetOrder order = withStatusCode_ $ \ierr ->
  [C.exp|void{gmshModelMeshSetOrder($(const int order), $(int * ierr))}|]

{- |
Get the last entities (if any) where a meshing error occurred. Currently
only populated by the new 3D meshing algorithms.
-}
gmshModelMeshGetLastEntityError :: IO (S.Vector Entity, StatusCode)
gmshModelMeshGetLastEntityError = withStatusCode $ \ierr ->
  parseEntitiesM $
    withCVector_ $ \dimTags dimTags_n ->
      [C.exp| void {
    gmshModelMeshGetLastEntityError
      ( $(int ** dimTags)
      , $(size_t * dimTags_n)
      , $(int * ierr)
      )}
  |]

{- |
Get the last entities (if any) where a meshing error occurred. Currently
only populated by the new 3D meshing algorithms.
-}
gmshModelMeshGetLastNodeError :: IO (S.Vector NodeTag, StatusCode)
gmshModelMeshGetLastNodeError = withStatusCode $ \ierr ->
  coerce $
    withCVector_ $ \nodeTags nodeTags_n ->
      [C.exp| void {
    gmshModelMeshGetLastNodeError
      ( $(size_t ** nodeTags)
      , $(size_t * nodeTags_n)
      , $(int * ierr)
      )}
  |]

{- |
Clear the mesh, i.e. delete all the nodes and elements, for the entities
@dimTags@. if @dimTags@ is empty, clear the whole mesh. Note that the mesh
of an entity can only be cleared if this entity is not on the boundary of
another entity with a non-empty mesh.
-}
gmshModelMeshClear :: S.Vector Entity -> IO StatusCode
gmshModelMeshClear (encodeEntities -> dimTags) =
  withStatusCode_ $ \ierr ->
    [C.exp| void {
    gmshModelMeshClear($vec-ptr:(int * dimTags), $vec-len:dimTags, 
      $(int * ierr))
    }|]

{- |
Get the nodes classified on the entity of dimension @dim@ and tag @tag@. If
@tag@ < 0, get the nodes for all entities of dimension @dim@. If @dim@ and
@tag@ are negative, get all the nodes in the mesh. @nodeTags@ contains the
node tags (their unique, strictly positive identification numbers). @coord@
is a vector of length 3 times the length of @nodeTags@ that contains the x,
y, z coordinates of the nodes, concatenated: [n1x, n1y, n1z, n2x, ...]. If
@dim@ >= 0 and @returnParamtricCoord@ is set, @parametricCoord@ contains
the parametric coordinates ([u1, u2, ...] or [u1, v1, u2, ...]) of the
nodes, if available. The length of @parametricCoord@ can be 0 or @dim@
times the length of @nodeTags@. If @includeBoundary@ is set, also return
the nodes classified on the boundary of the entity (which will be
reparametrized on the entity if @dim@ >= 0 in order to compute their
parametric coordinates).
-}
gmshModelMeshGetNodes ::
  Entity ->
  -- | includeBoundary
  Bool ->
  -- | returnParametricCoord
  Bool ->
  IO ((S.Vector NodeTag, S.Vector Coord, S.Vector CDouble), StatusCode)
gmshModelMeshGetNodes
  (CEntity dim tag)
  (bool -> includeBoundary)
  (bool -> returnParametricCoord) = withStatusCode $ \ierr -> do
    (nodeTags, (coords, paramCoords)) <-
      withCVector $ \nodeTags nodeTags_n ->
        withCVector $ \coord coord_n ->
          withCVector_ $ \parametricCoord parametricCoord_n ->
            [C.exp| void {
      gmshModelMeshGetNodes
        ( $(size_t ** nodeTags)
        , $(size_t * nodeTags_n)
        , $(double ** coord)
        , $(size_t * coord_n)
        , $(double ** parametricCoord)
        , $(size_t * parametricCoord_n)
        , $(const int dim)
        , $(const int tag)
        , $(const int includeBoundary)
        , $(const int returnParametricCoord)
        , $(int * ierr)
        )
    }|]
    pure (coerce nodeTags, parseCoords coords, paramCoords)

{- |
Get the elements classified on the entity of dimension @dim@ and tag @tag@.
If @tag@ < 0, get the elements for all entities of dimension @dim@. If
@dim@ and @tag@ are negative, get all the elements in the mesh.
@elementTypes@ contains the MSH types of the elements (e.g. @2@ for 3-node
triangles: see @getElementProperties@ to obtain the properties for a given
element type). @elementTags@ is a vector of the same length as
@elementTypes@; each entry is a vector containing the tags (unique,
strictly positive identifiers) of the elements of the corresponding type.
@nodeTags@ is also a vector of the same length as @elementTypes@; each
entry is a vector of length equal to the number of elements of the given
type times the number N of nodes for this type of element, that contains
the node tags of all the elements of the given type, concatenated: [e1n1,
e1n2, ..., e1nN, e2n1, ...].
-}
gmshModelMeshGetElements ::
  Dimension ->
  EntityTag ->
  -- | @(elementTypes, elementTags, nodeTags)@
  IO
    ( ( S.Vector ElementType
      , V.Vector (S.Vector ElementTag)
      , V.Vector (S.Vector NodeTag)
      )
    , StatusCode
    )
gmshModelMeshGetElements (Dimension dim) (EntityTag tag) =
  withStatusCode $ \ierr -> do
    (a, (b, c)) <- withCVector $ \elementTypes elementTypes_n ->
      withNestedVector $ \elementTags elementTags_n elementTags_nn ->
        withNestedVector_ $ \nodeTags nodeTags_n nodeTags_nn ->
          [C.exp|void  {
        gmshModelMeshGetElements
          ( $(int ** elementTypes)
          , $(size_t * elementTypes_n)
          , $(size_t *** elementTags)
          , $(size_t ** elementTags_n)
          , $(size_t * elementTags_nn)
          , $(size_t *** nodeTags)
          , $(size_t ** nodeTags_n)
          , $(size_t * nodeTags_nn)
          , $(const int dim)
          , $(const int tag)
          , $(int * ierr)
          )}
        |]
    pure (coerce a, coerce b, coerce c)

{- |
Get the type and node tags of the element with tag @tag@. This function
relies on an internal cache (a vector in case of dense element numbering, a
map otherwise); for large meshes accessing elements in bulk is often
preferable.
-}
gmshModelMeshGetElement ::
  ElementTag -> IO ((ElementType, S.Vector NodeTag), StatusCode)
gmshModelMeshGetElement
  (ElementTag elementTag) =
    withStatusCode $ \ierr -> coerce $
      withPtr $ \elementType ->
        withCVector_ $ \nodeTags nodeTags_n ->
          [C.exp| void {
        gmshModelMeshGetElement(
            $(const size_t elementTag),
            $(int * elementType),
            $(size_t ** nodeTags), $(size_t * nodeTags_n),
            $(int * ierr))
      } |]

{- |
Set a transfinite meshing constraint on the curve @tag@, with @numNodes@
nodes distributed according to @meshType@ and @coef@. Currently supported
types are @"Progression"@ (geometrical progression with power @coef@), @"Bump"@
(refinement toward both extremities of the curve) and @"Beta"@ (beta law).
-}
gmshModelMeshSetTransfiniteCurve ::
  EntityTag ->
  -- | numNode
  CInt ->
  -- | meshType
  BS8.ByteString ->
  -- | coef
  CDouble ->
  IO StatusCode
gmshModelMeshSetTransfiniteCurve (EntityTag tag) numNode meshType coef =
  withStatusCode_ $ \ierr ->
    [C.exp|void {
          gmshModelMeshSetTransfiniteCurve
          ( $(const int tag)
          , $(const int numNode)
          , $bs-cstr:meshType
          , $(const double coef)
          , $(int * ierr)
          )
      }|]

{- |
Set a transfinite meshing constraint on the surface @tag@. @arrangement@
describes the arrangement of the triangles when the surface is not flagged
as recombined: currently supported values are @"Left"@, @"Right"@,
@"AlternateLeft"@ and @"AlternateRight"@. @cornerTags@ can be used to specify
the (3 or 4) corners of the transfinite interpolation explicitly;
specifying the corners explicitly is mandatory if the surface has more that
3 or 4 points on its boundary.
-}
gmshModelMeshSetTransfiniteSurface ::
  EntityTag ->
  BS8.ByteString ->
  S.Vector EntityTag ->
  IO StatusCode
gmshModelMeshSetTransfiniteSurface
  (EntityTag tag)
  arrangement
  (S.unsafeCast @EntityTag -> cornerTags) =
    withStatusCode_ $ \ierr ->
      [C.exp|void {
      gmshModelMeshSetTransfiniteSurface
        ( $(const int tag)
        , $bs-cstr:arrangement
        , $vec-ptr:(int * cornerTags)
        , $vec-len:cornerTags
        , $(int * ierr)
        )
  }|]

{- | Set a transfinite meshing constraint on the surface @tag@. @cornerTags@ can
be used to specify the (6 or 8) corners of the transfinite interpolation
explicitly.

NOTE: above comment is stolen from header file; it seems @surface@ must be @volume@.
-}
gmshModelMeshSetTransfiniteVolume ::
  EntityTag -> S.Vector EntityTag -> IO StatusCode
gmshModelMeshSetTransfiniteVolume (EntityTag tag) (S.unsafeCast @EntityTag -> cornerTags) = withStatusCode_ $ \ierr ->
  [C.exp|void {
    gmshModelMeshSetTransfiniteVolume
      ( $(const int tag)
      , $vec-ptr:(int * cornerTags)
      , $vec-len:cornerTags
      , $(int * ierr)
      ) }
    |]

{- |
Set transfinite meshing constraints on the model entities in @dimTag@.
Transfinite meshing constraints are added to the curves of the quadrangular
surfaces and to the faces of 6-sided volumes. Quadragular faces with a
corner angle superior to @cornerAngle@ (in radians) are ignored. The number
of points is automatically determined from the sizing constraints. If
@dimTag@ is empty, the constraints are applied to all entities in the
model. If @recombine@ is true, the recombine flag is automatically set on
the transfinite surfaces.
-}
gmshModelMeshSetTransfiniteAutomatic :: S.Vector Entity -> CDouble -> Bool -> IO StatusCode
gmshModelMeshSetTransfiniteAutomatic
  (encodeEntities -> dimTags)
  cornerAngle
  (bool -> recombine) =
    withStatusCode_ $ \ierr ->
      [C.exp|void {
          gmshModelMeshSetTransfiniteAutomatic
            ( $vec-ptr:(int * dimTags)
            , $vec-len:dimTags
            , $(const double cornerAngle)
            , $(const int recombine)
            , $(int * ierr)
            )}|]

{- |
Set a recombination meshing constraint on the model entity of dimension
@dim@ and tag @tag@. Currently only entities of dimension 2 (to recombine
triangles into quadrangles) are supported.
-}
gmshModelMeshSetRecombine :: Entity -> IO StatusCode
gmshModelMeshSetRecombine (CEntity dim tag) =
  withStatusCode_ $ \ierr ->
    [C.exp| void {
      gmshModelMeshSetRecombine
        ( $(const int dim)
        , $(const int tag)
        , $(int * ierr)
        )}|]

{- |
Synchronize the built-in CAD representation with the current Gmsh model.
This can be called at any time, but since it involves a non trivial amount
of processing, the number of synchronization points should normally be
minimized. Without synchronization the entities in the built-in CAD
representation are not available to any function outside of the built-in
CAD kernel functions.
-}
gmshModelGeoSynchronize :: IO StatusCode
gmshModelGeoSynchronize = withStatusCode_ $ \ierr ->
  [C.exp|void { gmshModelGeoSynchronize($(int *ierr)) }|]

{- |
Synchronize the built-in CAD representation with the current Gmsh model.
This can be called at any time, but since it involves a non trivial amount
of processing, the number of synchronization points should normally be
minimized. Without synchronization the entities in the built-in CAD
representation are not available to any function outside of the built-in
CAD kernel functions.
-}
gmshModelOccSynchronize :: IO StatusCode
gmshModelOccSynchronize = withStatusCode_ $ \ierr ->
  [C.exp|void { gmshModelOccSynchronize($(int *ierr)) }|]

{- |
Add a geometrical point in the built-in CAD representation, at coordinates
(@x@, @y@, @z@). If @meshSize@ is > 0, add a meshing constraint at that
point. If @tag@ is positive, set the tag explicitly; otherwise a new tag is
selected automatically. Return the tag of the point. (Note that the point
will be added in the current model only after @synchronize@ is called. This
behavior holds for all the entities added in the geo module.)
-}
gmshModelGeoAddPoint ::
  Coord -> CDouble -> EntityTag -> IO (EntityTag, StatusCode)
gmshModelGeoAddPoint (Coord (Vec3 x y z)) meshSize (EntityTag tag) =
  withStatusCode $ \ierr ->
    EntityTag
      <$> [C.exp| int {
     gmshModelGeoAddPoint
      ( $(const double x),
        $(const double y),
        $(const double z),
        $(const double meshSize),
        $(const int tag),
        $(int * ierr)
    )}|]

{- |
Add a straight line segment in the built-in CAD representation, between the
two points with tags @startTag@ and @endTag@. If @tag@ is positive, set the
tag explicitly; otherwise a new tag is selected automatically. Return the
tag of the line.
-}
gmshModelGeoAddLine ::
  -- | startTag
  EntityTag ->
  -- | endTag
  EntityTag ->
  -- | tag
  EntityTag ->
  IO (EntityTag, StatusCode)
gmshModelGeoAddLine (EntityTag startTag) (EntityTag endTag) (EntityTag tag) =
  withStatusCode $ \ierr ->
    coerce
      [C.exp| int {
      gmshModelGeoAddLine
        ( $(const int startTag)
        , $(const int endTag)
        , $(const int tag)
        , $(int * ierr)
        )
    }|]

{- |
Add a circle arc (strictly smaller than Pi) in the built-in CAD
representation, between the two points with tags @startTag@ and @endTag@,
and with center @centerTag@. If @tag@ is positive, set the tag explicitly;
otherwise a new tag is selected automatically. If (@nx@, @ny@, @nz@) != (0,
0, 0), explicitly set the plane of the circle arc. Return the tag of the
circle arc.
-}
gmshModelGeoAddCircleArc ::
  -- | startTag
  EntityTag ->
  -- | centerTag
  EntityTag ->
  -- | endTag
  EntityTag ->
  -- | tag
  EntityTag ->
  -- | (nx, ny, nz)
  Vec3 ->
  IO (EntityTag, StatusCode)
gmshModelGeoAddCircleArc
  (EntityTag startTag)
  (EntityTag centerTag)
  (EntityTag endTag)
  (EntityTag tag)
  (Vec3 nx ny nz) =
    withStatusCode $ \ierr ->
      EntityTag
        <$> [C.exp| int {
              gmshModelGeoAddCircleArc
                ( $(const int startTag)
                , $(const int centerTag)
                , $(const int endTag)
                , $(const int tag)
                , $(const double nx)
                , $(const double ny)
                , $(const double nz)
                , $(int * ierr)
                )
            }|]

{- |
Add an ellipse arc (strictly smaller than Pi) in the built-in CAD
representation, between the two points @startTag@ and @endTag@, and with
center @centerTag@ and major axis point @majorTag@. If @tag@ is positive,
set the tag explicitly; otherwise a new tag is selected automatically. If
(@nx@, @ny@, @nz@) != (0, 0, 0), explicitly set the plane of the circle
arc. Return the tag of the ellipse arc.
-}
gmshModelGeoAddEllipseArc ::
  -- | startTag
  EntityTag ->
  -- | centerTag
  EntityTag ->
  -- | majorTag
  EntityTag ->
  -- | endTag
  EntityTag ->
  -- | tag
  EntityTag ->
  -- | @(nx,ny,nz)@
  Vec3 ->
  IO (EntityTag, StatusCode)
gmshModelGeoAddEllipseArc
  (EntityTag startTag)
  (EntityTag centerTag)
  (EntityTag majorTag)
  (EntityTag endTag)
  (EntityTag tag)
  (Vec3 nx ny nz) =
    withStatusCode $ \ierr ->
      EntityTag
        <$> [C.exp| int {
              gmshModelGeoAddEllipseArc
                ( $(const int startTag)
                , $(const int centerTag)
                , $(const int majorTag)
                , $(const int endTag)
                , $(const int tag)
                , $(const double nx)
                , $(const double ny)
                , $(const double nz)
                , $(int * ierr)
                )
            }|]

{- |
Add a spline (Catmull-Rom) curve in the built-in CAD representation, going
through the points @pointTags@. If @tag@ is positive, set the tag
explicitly; otherwise a new tag is selected automatically. Create a
periodic curve if the first and last points are the same. Return the tag of
the spline curve.
-}
gmshModelGeoAddSpline ::
  -- | pointTags
  S.Vector EntityTag ->
  -- | tag
  EntityTag ->
  IO (EntityTag, StatusCode)
gmshModelGeoAddSpline
  (S.unsafeCast @EntityTag -> pointTags)
  (EntityTag tag) =
    withStatusCode $ \ierr ->
      EntityTag
        <$> [C.exp| int {
              gmshModelGeoAddSpline
                ( $vec-ptr:(int * pointTags)
                , $vec-len:pointTags
                , $(const int tag)
                , $(int * ierr)
                )
            }|]

{- |
Add a b-spline curve in the built-in CAD representation, with
@pointTags@ control points. If @tag@ is positive, set the tag
explicitly; otherwise a new tag is selected automatically. Creates a
periodic curve if the first and last points are the same. Return the tag of
the b-spline curve.
-}
gmshModelGeoAddBSpline ::
  -- | pointTags
  S.Vector EntityTag ->
  -- | tag
  EntityTag ->
  IO (EntityTag, StatusCode)
gmshModelGeoAddBSpline
  (S.unsafeCast @EntityTag -> pointTags)
  (EntityTag tag) =
    withStatusCode $ \ierr ->
      EntityTag
        <$> [C.exp| int {
              gmshModelGeoAddBSpline
                ( $vec-ptr:(int * pointTags)
                , $vec-len:pointTags
                , $(const int tag)
                , $(int * ierr)
                )
            }|]

{- |
Add a Bezier curve in the built-in CAD representation, with
@pointTags@ control points. If @tag@ is positive, set the tag
explicitly; otherwise a new tag is selected automatically. Creates a
periodic curve if the first and last points are the same. Return the tag of
the Bezier curve.
-}
gmshModelGeoAddBezier ::
  -- | pointTags
  S.Vector EntityTag ->
  -- | tag
  EntityTag ->
  IO (EntityTag, StatusCode)
gmshModelGeoAddBezier
  (S.unsafeCast @EntityTag -> pointTags)
  (EntityTag tag) =
    withStatusCode $ \ierr ->
      EntityTag
        <$> [C.exp| int {
              gmshModelGeoAddBezier
                ( $vec-ptr:(int * pointTags)
                , $vec-len:pointTags
                , $(const int tag)
                , $(int * ierr)
                )
            }|]

{- |
Add a polyline curve in the built-in CAD representation, going through the
points @pointTags@. If @tag@ is positive, set the tag
explicitly; otherwise a new tag is selected automatically. Creates a
periodic curve if the first and last points are the same. Return the tag of
the polyline curve.
-}
gmshModelGeoAddPolyline ::
  -- | pointTags
  S.Vector EntityTag ->
  -- | tag
  EntityTag ->
  IO (EntityTag, StatusCode)
gmshModelGeoAddPolyline
  (S.unsafeCast @EntityTag -> pointTags)
  (EntityTag tag) =
    withStatusCode $ \ierr ->
      EntityTag
        <$> [C.exp| int {
              gmshModelGeoAddPolyline
                ( $vec-ptr:(int * pointTags)
                , $vec-len:pointTags
                , $(const int tag)
                , $(int * ierr)
                )
            }|]

{- |
Add a spline (Catmull-Rom) curve in the built-in CAD representation, going
through points sampling the curves in @curveTags@. The density of sampling
points on each curve is governed by @numIntervals@. If @tag@ is positive,
set the tag explicitly; otherwise a new tag is selected automatically.
Return the tag of the spline.
-}
gmshModelGeoAddCompoundSpline ::
  -- | curveTags
  S.Vector EntityTag ->
  -- | numIntervals
  CInt ->
  -- | tag
  EntityTag ->
  IO (EntityTag, StatusCode)
gmshModelGeoAddCompoundSpline
  (S.unsafeCast @EntityTag -> curveTags)
  numIntervals
  (EntityTag tag) =
    withStatusCode $ \ierr ->
      EntityTag
        <$> [C.exp| int {
              gmshModelGeoAddCompoundSpline
                ( $vec-ptr:(int * curveTags)
                , $vec-len:curveTags
                , $(const int numIntervals)
                , $(const int tag)
                , $(int * ierr)
                )
            }|]

{- |
Add a b-spline curve in the built-in CAD representation, going
through points sampling the curves in @curveTags@. The density of sampling
points on each curve is governed by @numIntervals@. If @tag@ is positive,
set the tag explicitly; otherwise a new tag is selected automatically.
Return the tag of the b-spline.
-}
gmshModelGeoAddCompoundBSpline ::
  -- | curveTags
  S.Vector EntityTag ->
  -- | numIntervals
  CInt ->
  -- | tag
  EntityTag ->
  IO (EntityTag, StatusCode)
gmshModelGeoAddCompoundBSpline
  (S.unsafeCast @EntityTag -> curveTags)
  numIntervals
  (EntityTag tag) =
    withStatusCode $ \ierr ->
      EntityTag
        <$> [C.exp| int {
              gmshModelGeoAddCompoundBSpline
                ( $vec-ptr:(int * curveTags)
                , $vec-len:curveTags
                , $(const int numIntervals)
                , $(const int tag)
                , $(int * ierr)
                )
            }|]

{- |
Add a curve loop (a closed wire) in the built-in CAD representation, formed
by the curves @curveTags@. @curveTags@ should contain (signed) tags of
model entities of dimension 1 forming a closed loop: a negative tag
signifies that the underlying curve is considered with reversed
orientation. If @tag@ is positive, set the tag explicitly; otherwise a new
tag is selected automatically. If @reorient@ is set, automatically reorient
the curves if necessary. Return the tag of the curve loop.
-}
gmshModelGeoAddCurveLoop ::
  -- | @curveTags@
  S.Vector EntityTag ->
  -- | @tag@
  EntityTag ->
  -- | @reorient@
  Bool ->
  IO (EntityTag, StatusCode)
gmshModelGeoAddCurveLoop
  (S.unsafeCast @EntityTag -> curveTags)
  (EntityTag tag)
  (bool -> reorient) =
    withStatusCode $ \ierr ->
      EntityTag
        <$> [C.exp| int {
        gmshModelGeoAddCurveLoop
          ( $vec-ptr:(int * curveTags)
          , $vec-len:curveTags
          , $(const int tag)
          , $(const int reorient)
          , $(int * ierr)
          )
        }|]

{- |
Add curve loops in the built-in CAD representation based on the curves
@curveTags@. Return the @tags@ of found curve loops, if any.
-}
gmshModelGeoAddCurveLoops ::
  -- | @curveTags@
  S.Vector EntityTag ->
  IO (S.Vector EntityTag, StatusCode)
gmshModelGeoAddCurveLoops (S.unsafeCast @EntityTag -> curveTags) =
  withStatusCode $ \ierr -> coerce $
    withCVector_ $ \tags tags_n ->
      [C.exp| void {
        gmshModelGeoAddCurveLoops
          ( $vec-ptr:(int * curveTags)
          , $vec-len:curveTags
          , $(int ** tags)
          , $(size_t *tags_n)
          , $(int * ierr)
          )
        }|]

{- |
Add a plane surface in the built-in CAD representation, defined by one or
more curve loops @wireTags@. The first curve loop defines the exterior
contour; additional curve loop define holes. If @tag@ is positive, set the
tag explicitly; otherwise a new tag is selected automatically. Return the
tag of the surface.
-}
gmshModelGeoAddPlaneSurface ::
  -- | @wireTags@
  S.Vector EntityTag ->
  -- | @tag@
  EntityTag ->
  IO (EntityTag, StatusCode)
gmshModelGeoAddPlaneSurface
  (S.unsafeCast @EntityTag -> wireTags)
  (EntityTag tag) =
    withStatusCode $ \ierr ->
      EntityTag
        <$> [C.exp| int {
        gmshModelGeoAddPlaneSurface
          ( $vec-ptr:(int * wireTags)
          , $vec-len:wireTags
          , $(const int tag)
          , $(int * ierr)
          )
        }|]

{- |
Add a surface loop (a closed shell) formed by @surfaceTags@ in the built-in
CAD representation.  If @tag@ is positive, set the tag explicitly;
otherwise a new tag is selected automatically. Return the tag of the shell.
-}
gmshModelGeoAddSurfaceLoop ::
  -- | @surfaceTags@
  S.Vector EntityTag ->
  -- | @tag@
  EntityTag ->
  IO (EntityTag, StatusCode)
gmshModelGeoAddSurfaceLoop
  (S.unsafeCast @EntityTag -> surfaceTags)
  (EntityTag tag) =
    withStatusCode $ \ierr ->
      EntityTag
        <$> [C.exp| int {
        gmshModelGeoAddSurfaceLoop
          ( $vec-ptr:(int * surfaceTags)
          , $vec-len:surfaceTags
          , $(const int tag)
          , $(int * ierr)
          )
        }|]

{- |
Add a volume (a region) in the built-in CAD representation, defined by one
or more shells @shellTags@. The first surface loop defines the exterior
boundary; additional surface loop define holes. If @tag@ is positive, set
the tag explicitly; otherwise a new tag is selected automatically. Return
the tag of the volume.
-}
gmshModelGeoAddVolume ::
  -- | @shellTags@
  S.Vector EntityTag ->
  -- | @tag@
  EntityTag ->
  IO (EntityTag, StatusCode)
gmshModelGeoAddVolume
  (S.unsafeCast @EntityTag -> shellTags)
  (EntityTag tag) =
    withStatusCode $ \ierr ->
      EntityTag
        <$> [C.exp| int {
        gmshModelGeoAddVolume
          ( $vec-ptr:(int * shellTags)
          , $vec-len:shellTags
          , $(const int tag)
          , $(int * ierr)
          )
        }|]

{- |
Extrude the entities @entities@ in the built-in CAD representation, using a
translation along (@dx@, @dy@, @dz@). Return extruded entities.
If @numElements@ is not empty, also extrude the mesh: the
entries in @numElements@ give the number of elements in each layer. If
@height@ is not empty, it provides the (cumulative) height of the different
layers, normalized to 1. If @recombine@ is set, recombine the mesh in the
layers.
-}
gmshModelGeoExtrude ::
  -- | entities
  S.Vector Entity ->
  -- | @(dx, dy, dz)@
  Vec3 ->
  -- | @numElements@
  S.Vector CInt ->
  -- | @heights@
  S.Vector CDouble ->
  -- | @recombine@
  Bool ->
  IO (S.Vector Entity, StatusCode)
gmshModelGeoExtrude
  (encodeEntities -> dimTags)
  (Vec3 dx dy dz)
  numElements
  heights
  (bool -> recombine) =
    withStatusCode $ \ierr -> parseEntitiesM $
      withCVector_ $ \outDimTags outDimTags_n ->
        [C.exp|void {
          gmshModelGeoExtrude
            ( $vec-ptr:(int * dimTags)
            , $vec-len:dimTags
            , $(const double dx)
            , $(const double dy)
            , $(const double dz)
            , $(int ** outDimTags)
            , $(size_t * outDimTags_n)
            , $vec-ptr:(int * numElements)
            , $vec-len:numElements
            , $vec-ptr:(double * heights)
            , $vec-len:heights
            , $(const int recombine)
            , $(int * ierr)
            )
      } |]

{- |
Extrude the entities  in the built-in CAD representation, using a
rotation of @angle@ radians around the axis of revolution defined by the
point @center@ and the @direction@. The angle
should be strictly smaller than Pi. Return extruded entities. If @numElements@ is not empty, also extrude the mesh: the
entries in @numElements@ give the number of elements in each layer. If
@height@ is not empty, it provides the (cumulative) height of the different
layers, normalized to 1. If @recombine@ is set, recombine the mesh in the
layers.
-}
gmshModelGeoRevolve ::
  -- | entities
  S.Vector Entity ->
  -- | center
  Coord ->
  -- | axis direction
  Vec3 ->
  -- | angle
  CDouble ->
  -- | recombine?
  Bool ->
  -- | numElements
  S.Vector CInt ->
  -- | heights
  S.Vector CDouble ->
  IO (S.Vector Entity, StatusCode)
gmshModelGeoRevolve
  (encodeEntities -> dimTags)
  (Coord (Vec3 x y z))
  (Vec3 ax ay az)
  angle
  (bool -> recombine)
  numElements
  heights =
    withStatusCode $ \ierr -> parseEntitiesM $
      withCVector_ $ \outDimTags outDimTags_n ->
        [C.exp|void {
          gmshModelGeoRevolve(
            $vec-ptr:(int * dimTags), $vec-len:dimTags,
            $(const double x),
            $(const double y),
            $(const double z),
            $(const double ax),
            $(const double ay),
            $(const double az),
            $(const double angle),
            $(int ** outDimTags), $(size_t * outDimTags_n),
            $vec-ptr:(int * numElements), $vec-len:numElements,
            $vec-ptr:(double * heights), $vec-len:heights,
            $(const int recombine),
            $(int * ierr))
        }|]

-- | Copy the entities @entities@ in the built-in CAD representation; the new entities are returned.
gmshModelGeoCopy :: S.Vector Entity -> IO (S.Vector Entity, StatusCode)
gmshModelGeoCopy (encodeEntities -> entities) =
  withStatusCode $ \ierr -> parseEntitiesM $
    withCVector_ $ \outDimTags outDimTags_n ->
      [C.exp| void {
        gmshModelGeoCopy
          ( $vec-ptr:(int * entities)
          , $vec-len:entities
          , $(int ** outDimTags)
          , $(size_t *outDimTags_n)
          , $(int *ierr)
          )
      }|]

gmshModelGeoMirror ::
  S.Vector Entity -> CDouble -> CDouble -> CDouble -> CDouble -> IO StatusCode
gmshModelGeoMirror (encodeEntities -> dimTags) a b c d =
  withStatusCode_ $ \ierr ->
    [C.exp|void {
      gmshModelGeoMirror
        ( $vec-ptr:(int * dimTags)
        , $vec-len:dimTags
        , $(const double a)
        , $(const double b)
        , $(const double c)
        , $(const double d)
        , $(int * ierr)
        )
    }|]

gmshLoggerGetLastError ::
  IO (BS.ByteString, StatusCode)
gmshLoggerGetLastError = withStatusCode $ \ierr ->
  withCString_ $ \errMsg ->
    [C.exp| void {
      gmshLoggerGetLastError
        ( $(char ** errMsg)
        , $(int * ierr)
        )
      }|]

-- | Start logging messages.
gmshLoggerStart :: IO StatusCode
gmshLoggerStart = withStatusCode_ $ \ierr ->
  [C.exp|void {gmshLoggerStart($(int * ierr));}|]

-- | Stop logging messages.
gmshLoggerStop :: IO StatusCode
gmshLoggerStop = withStatusCode_ $ \ierr ->
  [C.exp|void {gmshLoggerStop($(int * ierr));}|]

gmshModelGeoRemoveAllDuplicates :: IO StatusCode
gmshModelGeoRemoveAllDuplicates = withStatusCode_ $ \ierr ->
  [C.exp|void {gmshModelGeoRemoveAllDuplicates($(int * ierr))}|]

{- |
Set a transfinite meshing constraint on the curve @tag@ in the built-in CAD
kernel representation, with @numNodes@ nodes distributed according to
@meshType@ and @coef@. Currently supported types are @"Progression"@
(geometrical progression with power @coef@) and @"Bump"@ (refinement toward
both extremities of the curve)
-}
gmshModelGeoMeshSetTransfiniteCurve ::
  -- |  @tag@
  EntityTag ->
  -- | @nPoints@
  CInt ->
  -- | @meshType@
  BS.ByteString ->
  -- | @coef@
  CDouble ->
  IO StatusCode
gmshModelGeoMeshSetTransfiniteCurve (EntityTag tag) nPoints meshType coef = withStatusCode_ $ \ierr ->
  [C.exp|void { 
    gmshModelGeoMeshSetTransfiniteCurve
    ( $(const int tag), $(const int nPoints)
    , $bs-cstr:meshType, $(const double coef)
    , $(int *ierr)
    )
  }|]

{- |
Set a transfinite meshing constraint on the surface @tag@ in the built-in
CAD kernel representation. @arrangement@ describes the arrangement of the
triangles when the surface is not flagged as recombined: currently
supported values are @"Left"@, @"Right"@, @"AlternateLeft"@ and @"AlternateRight"@.
@cornerTags@ can be used to specify the (3 or 4) corners of the transfinite
interpolation explicitly; specifying the corners explicitly is mandatory if
the surface has more that 3 or 4 points on its boundary.
-}
gmshModelGeoMeshSetTransfiniteSurface ::
  -- | @tag@
  EntityTag ->
  -- | @arrangement@
  BS.ByteString ->
  -- | @cornerTags@
  S.Vector EntityTag ->
  IO StatusCode
gmshModelGeoMeshSetTransfiniteSurface
  (EntityTag tag)
  arrangement
  (S.unsafeCast -> cornerTags) = withStatusCode_ $ \ierr ->
    [C.exp|void { 
    gmshModelGeoMeshSetTransfiniteSurface
    ( $(const int tag)
    , $bs-cstr:arrangement
    , $vec-ptr:(int *cornerTags), $vec-len:cornerTags
    , $(int *ierr)
    )
  }|]

{- |
Set a transfinite meshing constraint on the surface @tag@ in the built-in
CAD kernel representation. @cornerTags@ can be used to specify the (6 or 8)
corners of the transfinite interpolation explicitly.
-}
gmshModelGeoMeshSetTransfiniteVolume ::
  -- | @tag@
  EntityTag ->
  -- | @cornerTags@
  S.Vector EntityTag ->
  IO StatusCode
gmshModelGeoMeshSetTransfiniteVolume (EntityTag tag) (S.unsafeCast -> cornerTags) = withStatusCode_ $ \ierr ->
  [C.exp|void { 
    gmshModelGeoMeshSetTransfiniteVolume
    ( $(const int tag)
    , $vec-ptr:(int *cornerTags), $vec-len:cornerTags
    , $(int *ierr)
    )
  }|]

{- |
Set a recombination meshing constraint on the entity of dimension @dim@ and
tag @tag@ in the built-in CAD kernel representation. Currently only
entities of dimension 2 (to recombine triangles into quadrangles) are
supported.
-}
gmshModelGeoMeshSetRecombine ::
  -- | @dim@ and @tag@
  Entity ->
  -- | @angle@
  CDouble ->
  IO StatusCode
gmshModelGeoMeshSetRecombine (CEntity dim tag) angle = withStatusCode_ $ \ierr ->
  [C.exp|void {
    gmshModelGeoMeshSetRecombine
      ( $(const int dim), $(const int tag), $(const double angle)
      , $(int * ierr)
      )
  }|]

gmshModelOccRemoveAllDuplicates :: IO StatusCode
gmshModelOccRemoveAllDuplicates = withStatusCode_ $ \ierr ->
  [C.exp|void {gmshModelOccRemoveAllDuplicates($(int * ierr))}|]

gmshModelMeshRemoveDuplicateNodes :: IO StatusCode
gmshModelMeshRemoveDuplicateNodes = withStatusCode_ $ \ierr ->
  [C.exp|void {gmshModelMeshRemoveDuplicateNodes($(int * ierr))}|]

{- |
Add a geometrical point in the OpenCASCADE CAD representation, at
coordinates (@x@, @y@, @z@). If @meshSize@ is > 0, add a meshing constraint
at that point. If @tag@ is positive, set the tag explicitly; otherwise a
new tag is selected automatically. Return the tag of the point. (Note that
the point will be added in the current model only after @synchronize@ is
called. This behavior holds for all the entities added in the occ module.)
-}
gmshModelOccAddPoint :: Coord -> CDouble -> EntityTag -> IO (EntityTag, StatusCode)
gmshModelOccAddPoint (Coord (Vec3 x y z)) meshSize (EntityTag tag) =
  withStatusCode $ \ierr ->
    EntityTag
      <$> [C.exp|int {
    gmshModelOccAddPoint
    ( $(const double x)
    , $(const double y)
    , $(const double z)
    , $(const double meshSize)
    , $(const int tag)
    , $(int * ierr)
    )
  }|]

{- |
Add a straight line segment in the OpenCASCADE CAD representation, between the
two points with tags @startTag@ and @endTag@. If @tag@ is positive, set the
tag explicitly; otherwise a new tag is selected automatically. Return the
tag of the line.
-}
gmshModelOccAddLine ::
  -- | startTag
  EntityTag ->
  -- | endTag
  EntityTag ->
  -- | tag
  EntityTag ->
  IO (EntityTag, StatusCode)
gmshModelOccAddLine (EntityTag startTag) (EntityTag endTag) (EntityTag tag) =
  withStatusCode $ \ierr ->
    coerce
      [C.exp| int {
      gmshModelOccAddLine
        ( $(const int startTag)
        , $(const int endTag)
        , $(const int tag)
        , $(int * ierr)
        )
    }|]

{- |
Add a circle arc in the OpenCASCADE CAD representation, between the
two points with tags @startTag@ and @endTag@, with center @centerTag@. If @tag@
is positive, set the tag explicitly; otherwise a new tag is selected
automatically. Return the tag of the circle arc.
-}
gmshModelOccAddCircleArc ::
  -- | startTag
  EntityTag ->
  -- | centerTag
  EntityTag ->
  -- | endTag
  EntityTag ->
  -- | tag
  EntityTag ->
  IO (EntityTag, StatusCode)
gmshModelOccAddCircleArc
  (EntityTag startTag)
  (EntityTag centerTag)
  (EntityTag endTag)
  (EntityTag tag) =
    withStatusCode $ \ierr ->
      EntityTag
        <$> [C.exp| int {
              gmshModelOccAddCircleArc
                ( $(const int startTag)
                , $(const int centerTag)
                , $(const int endTag)
                , $(const int tag)
                , $(int * ierr)
                )
            }|]

{- |
Add an ellipse arc in the OpenCASCADE CAD representation, between the two
points @startTag@ and @endTag@, and with center @centerTag@ and major axis
point @majorTag@. If @tag@ is positive, set the tag explicitly; otherwise a
new tag is selected automatically. Return the tag of the ellipse arc. Note
that OpenCASCADE does not allow creating ellipse arcs with the major radius
smaller than the minor radius.
-}
gmshModelOccAddEllipseArc ::
  -- | startTag
  EntityTag ->
  -- | centerTag
  EntityTag ->
  -- | majorTag
  EntityTag ->
  -- | endTag
  EntityTag ->
  -- | tag
  EntityTag ->
  IO (EntityTag, StatusCode)
gmshModelOccAddEllipseArc
  (EntityTag startTag)
  (EntityTag centerTag)
  (EntityTag majorTag)
  (EntityTag endTag)
  (EntityTag tag) =
    withStatusCode $ \ierr ->
      EntityTag
        <$> [C.exp| int {
              gmshModelOccAddEllipseArc
                ( $(const int startTag)
                , $(const int centerTag)
                , $(const int majorTag)
                , $(const int endTag)
                , $(const int tag)
                , $(int * ierr)
                )
            }|]

{- |
Add a circle of center (@x@, @y@, @z@) and radius @r@ in the OpenCASCADE
CAD representation. If @tag@ is positive, set the tag explicitly; otherwise
a new tag is selected automatically. If @angle1@ and @angle2@ are
specified, create a circle arc between the two angles. Return the tag of
the circle.
-}
gmshModelOccAddCircle ::
  -- | @center@
  Coord ->
  -- | @r@, the radius
  CDouble ->
  -- | @tag@
  EntityTag ->
  -- | @angle1@
  CDouble ->
  -- | @angle2@
  CDouble ->
  IO (EntityTag, StatusCode)
gmshModelOccAddCircle (Coord (Vec3 x y z)) r (EntityTag tag) angle1 angle2 =
  withStatusCode $ \ierr ->
    EntityTag
      <$> [C.exp|int {
      gmshModelOccAddCircle
      ( $(const double x),
        $(const double y),
        $(const double z),
        $(const double r),
        $(const int tag),
        $(const double angle1),
        $(const double angle2),
        $(int * ierr)
      )
    } |]

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
gmshModelOccAddEllipse ::
  -- | @center@
  Coord ->
  -- | @r1@
  CDouble ->
  -- | @r2@
  CDouble ->
  -- | @tag@
  EntityTag ->
  -- | @angle1@
  CDouble ->
  -- | @angle2@
  CDouble ->
  IO (EntityTag, StatusCode)
gmshModelOccAddEllipse (Coord (Vec3 x y z)) r1 r2 (EntityTag tag) angle1 angle2 =
  withStatusCode $ \ierr ->
    EntityTag
      <$> [C.exp|int {
      gmshModelOccAddEllipse
      ( $(const double x),
        $(const double y),
        $(const double z),
        $(const double r1),
        $(const double r2),
        $(const int tag),
        $(const double angle1),
        $(const double angle2),
        $(int * ierr)
      )
    } |]

{- |
Add a spline (C2 b-spline) curve in the OpenCASCADE CAD representation,
going through the points @pointTags@. If @tag@ is positive, set the tag
explicitly; otherwise a new tag is selected automatically. Create a
periodic curve if the first and last points are the same. Return the tag of
the spline curve.
-}
gmshModelOccAddSpline ::
  S.Vector EntityTag -> EntityTag -> IO (EntityTag, StatusCode)
gmshModelOccAddSpline (S.unsafeCast @EntityTag -> pointTags) (EntityTag tag) =
  withStatusCode $ \ierr ->
    EntityTag
      <$> [C.exp|int {
      gmshModelOccAddSpline
      ( $vec-ptr:(int * pointTags),
        $vec-len:pointTags,
        $(const int tag),
        $(int * ierr)
      )
    } |]

{- |
Add a Bezier curve in the OpenCASCADE CAD representation, with @pointTags@
control points. If @tag@ is positive, set the tag explicitly; otherwise a
new tag is selected automatically. Return the tag of the Bezier curve.
-}
gmshModelOccAddBezier ::
  S.Vector EntityTag -> EntityTag -> IO (EntityTag, StatusCode)
gmshModelOccAddBezier (S.unsafeCast @EntityTag -> pointTags) (EntityTag tag) =
  withStatusCode $ \ierr ->
    EntityTag
      <$> [C.exp|int {
      gmshModelOccAddBezier
      ( $vec-ptr:(int * pointTags),
        $vec-len:pointTags,
        $(const int tag),
        $(int * ierr)
      )
    } |]

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
gmshModelOccAddCurveLoop ::
  -- | @curveTags@
  S.Vector EntityTag ->
  -- | @tag@
  EntityTag ->
  IO (EntityTag, StatusCode)
gmshModelOccAddCurveLoop
  (S.unsafeCast @EntityTag -> curveTags)
  (EntityTag tag) =
    withStatusCode $ \ierr ->
      EntityTag
        <$> [C.exp| int {
        gmshModelOccAddCurveLoop
          ( $vec-ptr:(int * curveTags)
          , $vec-len:curveTags
          , $(const int tag)
          , $(int * ierr)
          )
        }|]

{- |
Add a plane surface in the OpenCASCADE CAD representation, defined by one
or more curve loops (or closed wires) @wireTags@. The first curve loop
defines the exterior contour; additional curve loop define holes. If @tag@
is positive, set the tag explicitly; otherwise a new tag is selected
automatically. Return the tag of the surface.
-}
gmshModelOccAddPlaneSurface ::
  -- | @wireTags@
  S.Vector EntityTag ->
  -- | @tag@
  EntityTag ->
  IO (EntityTag, StatusCode)
gmshModelOccAddPlaneSurface
  (S.unsafeCast @EntityTag -> surfaceTags)
  (EntityTag tag) =
    withStatusCode $ \ierr ->
      EntityTag
        <$> [C.exp| int {
        gmshModelOccAddPlaneSurface
          ( $vec-ptr:(int * surfaceTags)
          , $vec-len:surfaceTags
          , $(const int tag)
          , $(int * ierr)
          )
      }|]

{- |
Add a surface loop (a closed shell) in the OpenCASCADE CAD representation,
formed by @surfaceTags@.  If @tag@ is positive, set the tag explicitly;
otherwise a new tag is selected automatically. Return the tag of the
surface loop. Setting @sewing@ allows to build a shell made of surfaces
that share geometrically identical (but topologically different) curves.
-}
gmshModelOccAddSurfaceLoop ::
  S.Vector EntityTag ->
  EntityTag ->
  -- | @sewing@
  Bool ->
  IO (EntityTag, StatusCode)
gmshModelOccAddSurfaceLoop
  (S.unsafeCast -> surfaceTags)
  (EntityTag tag)
  (bool -> sewing) =
    withStatusCode $ \ierr ->
      EntityTag
        <$> [C.exp| int {
            gmshModelOccAddSurfaceLoop
              ( $vec-ptr:(int * surfaceTags)
              , $vec-len:surfaceTags
              , $(const int tag)
              , $(const int sewing)
              , $(int * ierr)
              )
          }|]

{- |
Add a volume (a region) in the OpenCASCADE CAD representation, defined by one
or more surface loops @shellTags@. The first surface loop defines the exterior
boundary; additional surface loop define holes. If @tag@ is positive, set
the tag explicitly; otherwise a new tag is selected automatically. Return
the tag of the volume.
-}
gmshModelOccAddVolume ::
  -- | @shellTags@
  S.Vector EntityTag ->
  -- | @tag@
  EntityTag ->
  IO (EntityTag, StatusCode)
gmshModelOccAddVolume
  (S.unsafeCast @EntityTag -> shellTags)
  (EntityTag tag) =
    withStatusCode $ \ierr ->
      EntityTag
        <$> [C.exp| int {
        gmshModelOccAddVolume
          ( $vec-ptr:(int * shellTags)
          , $vec-len:shellTags
          , $(const int tag)
          , $(int * ierr)
          )
        }|]

{- |
Add a cylinder in the OpenCASCADE CAD representation, defined by the center
(@x@, @y@, @z@) of its first circular face, the 3 components (@dx@, @dy@,
@dz@) of the vector defining its axis and its radius @r@. The optional
@angle@ argument defines the angular opening (from 0 to 2*Pi). If @tag@ is
positive, set the tag explicitly; otherwise a new tag is selected
automatically. Return the tag of the cylinder.
-}
gmshModelOccAddCylinder ::
  -- | @(x, y, z)@
  Coord ->
  -- | @(dx, dy, dz)@
  Vec3 ->
  -- | @r@
  CDouble ->
  -- | @tag@
  EntityTag ->
  -- | @angle@
  CDouble ->
  IO (EntityTag, StatusCode)
gmshModelOccAddCylinder
  (Coord (Vec3 x y z))
  (Vec3 dx dy dz)
  r
  (EntityTag tag)
  angle =
    withStatusCode $ \ierr ->
      EntityTag
        <$> [C.exp| int {
              gmshModelOccAddCylinder
                ( $(const double x)
                , $(const double y)
                , $(const double z)
                , $(const double dx)
                , $(const double dy)
                , $(const double dz)
                , $(const double r)
                , $(const int tag)
                , $(const double angle)
                , $(int * ierr)
                )
              }|]

{- |
Extrude the entities @entities@ in the OpenCASCADE CAD representation, using a
translation along (@dx@, @dy@, @dz@). Return extruded entities. If @numElements@ is not empty, also extrude the mesh: the
entries in @numElements@ give the number of elements in each layer. If
@height@ is not empty, it provides the (cumulative) height of the different
layers, normalized to 1. If @recombine@ is set, recombine the mesh in the
layers.
-}
gmshModelOccExtrude ::
  -- | entities
  S.Vector Entity ->
  -- | @(dx, dy, dz)@
  Vec3 ->
  -- | @numElements@
  S.Vector CInt ->
  -- | @heights@
  S.Vector CDouble ->
  -- | @recombine@
  Bool ->
  IO (S.Vector Entity, StatusCode)
gmshModelOccExtrude
  (encodeEntities -> dimTags)
  (Vec3 dx dy dz)
  numElements
  heights
  (bool -> recombine) =
    withStatusCode $ \ierr -> parseEntitiesM $
      withCVector_ $ \outDimTags outDimTags_n ->
        [C.exp|void {
          gmshModelOccExtrude
            ( $vec-ptr:(int * dimTags)
            , $vec-len:dimTags
            , $(const double dx)
            , $(const double dy)
            , $(const double dz)
            , $(int ** outDimTags)
            , $(size_t * outDimTags_n)
            , $vec-ptr:(int * numElements)
            , $vec-len:numElements
            , $vec-ptr:(double * heights)
            , $vec-len:heights
            , $(const int recombine)
            , $(int * ierr)
            )
      } |]

{- |
Extrude the entities  in the OpenCASCADE CAD representation, using a
rotation of @angle@ radians around the axis of revolution defined by the
point @center@ and the @direction@. The angle
should be strictly smaller than Pi. Return extruded entities. If @numElements@ is not empty, also extrude the mesh: the
entries in @numElements@ give the number of elements in each layer. If
@height@ is not empty, it provides the (cumulative) height of the different
layers, normalized to 1. If @recombine@ is set, recombine the mesh in the
layers.
-}
gmshModelOccRevolve ::
  -- | entities
  S.Vector Entity ->
  -- | center
  Coord ->
  -- | axis direction
  Vec3 ->
  -- | angle
  CDouble ->
  -- | recombine?
  Bool ->
  -- | numElements
  S.Vector CInt ->
  -- | heights
  S.Vector CDouble ->
  IO (S.Vector Entity, StatusCode)
gmshModelOccRevolve
  (encodeEntities -> dimTags)
  (Coord (Vec3 x y z))
  (Vec3 ax ay az)
  angle
  (bool -> recombine)
  numElements
  heights =
    withStatusCode $ \ierr -> parseEntitiesM $
      withCVector_ $ \outDimTags outDimTags_n ->
        [C.exp|void {
          gmshModelOccRevolve(
            $vec-ptr:(int * dimTags), $vec-len:dimTags,
            $(const double x),
            $(const double y),
            $(const double z),
            $(const double ax),
            $(const double ay),
            $(const double az),
            $(const double angle),
            $(int ** outDimTags), $(size_t * outDimTags_n),
            $vec-ptr:(int * numElements), $vec-len:numElements,
            $vec-ptr:(double * heights), $vec-len:heights,
            $(const int recombine),
            $(int * ierr))
        }|]

{- |
Rotate the entities @entities@ in the OpenCASCADE CAD representation by
@angle@ radians around the axis of revolution defined by the point (@x@,
@y@, @z@) and the direction (@ax@, @ay@, @az@).
-}
gmshModelOccRotate ::
  S.Vector Entity ->
  Coord ->
  Vec3 ->
  -- | angle
  CDouble ->
  IO StatusCode
gmshModelOccRotate
  (encodeEntities -> entities)
  (Coord (Vec3 x y z))
  (Vec3 ax ay az)
  angle =
    withStatusCode_ $ \ierr ->
      [C.exp| void {
        gmshModelOccRotate
        ( $vec-ptr:(int * entities)
        , $vec-len:entities
        , $(const double x)
        , $(const double y)
        , $(const double z)
        , $(const double ax)
        , $(const double ay)
        , $(const double az)
        , $(const double angle)
        , $(int * ierr)
        )
        }|]

{- |
Compute the boolean union (the fusion) between the entities @objects@ and
@tools@ in the OpenCASCADE CAD representation. If @tag@ is positive, try to set the tag
explicitly (only valid if the boolean operation results in a single
entity). Remove the object if @removeObject@ is set. Remove the tool if
@removeTool@ is set.

Return the resulting entities in @outDimTags@ and parent-child dictionaryAs @outDimTagDic@.
@outDimTagDic@ orders @objects@ and then @tools@, associating their child entities as a vector.
-}
gmshModelOccFuse ::
  -- | objects
  S.Vector Entity ->
  -- | tools
  S.Vector Entity ->
  -- | tag
  EntityTag ->
  -- | removeObject
  Bool ->
  -- | removeTool
  Bool ->
  -- | @(outDimTags, outDimTagDic)@
  IO ((S.Vector Entity, V.Vector (S.Vector Entity)), StatusCode)
gmshModelOccFuse
  (encodeEntities -> objects)
  (encodeEntities -> tools)
  (EntityTag tag)
  (bool -> removeObject)
  (bool -> removeTool) =
    withStatusCode $ \ierr -> fmap (parseEntities *** V.map S.unsafeCast) $
      withCVector $ \outDimTags outDimTags_n ->
        withNestedVector_ $ \outDimTagsMap outDimTagsMap_n outDimTagsMap_nn ->
          [C.exp| void {
            gmshModelOccFuse
              ( $vec-ptr:(int * objects)
              , $vec-len:objects
              , $vec-ptr:(int * tools)
              , $vec-len:tools
              , $(int ** outDimTags)
              , $(size_t * outDimTags_n)
              , $(int *** outDimTagsMap)
              , $(size_t ** outDimTagsMap_n)
              , $(size_t * outDimTagsMap_nn)
              , $(const int tag)
              , $(const int removeObject)
              , $(const int removeTool)
              , $(int * ierr)
              )
            }|]

{- |
Compute the boolean intersection (the common parts) between the entities @objects@ and
@tools@ in the OpenCASCADE CAD representation. If @tag@ is positive, try to set the tag
explicitly (only valid if the boolean operation results in a single
entity). Remove the object if @removeObject@ is set. Remove the tool if
@removeTool@ is set.

Return the resulting entities in @outDimTags@ and parent-child dictionaryAs @outDimTagDic@.
@outDimTagDic@ orders @objects@ and then @tools@, associating their child entities as a vector.
-}
gmshModelOccIntersect ::
  -- | objects
  S.Vector Entity ->
  -- | tools
  S.Vector Entity ->
  -- | tag
  EntityTag ->
  -- | removeObject
  Bool ->
  -- | removeTool
  Bool ->
  -- | @(outDimTags, outDimTagDic)@
  IO ((S.Vector Entity, V.Vector (S.Vector Entity)), StatusCode)
gmshModelOccIntersect
  (encodeEntities -> objects)
  (encodeEntities -> tools)
  (EntityTag tag)
  (bool -> removeObject)
  (bool -> removeTool) =
    withStatusCode $ \ierr -> fmap (parseEntities *** V.map S.unsafeCast) $
      withCVector $ \outDimTags outDimTags_n ->
        withNestedVector_ $ \outDimTagsMap outDimTagsMap_n outDimTagsMap_nn ->
          [C.exp| void {
            gmshModelOccIntersect
              ( $vec-ptr:(int * objects)
              , $vec-len:objects
              , $vec-ptr:(int * tools)
              , $vec-len:tools
              , $(int ** outDimTags)
              , $(size_t * outDimTags_n)
              , $(int *** outDimTagsMap)
              , $(size_t ** outDimTagsMap_n)
              , $(size_t * outDimTagsMap_nn)
              , $(const int tag)
              , $(const int removeObject)
              , $(const int removeTool)
              , $(int * ierr)
              )
            }|]

{- |
Compute the boolean difference between the entities @objects@ and
@tools@ in the OpenCASCADE CAD representation. If @tag@ is positive, try to set the tag
explicitly (only valid if the boolean operation results in a single
entity). Remove the object if @removeObject@ is set. Remove the tool if
@removeTool@ is set.

Return the resulting entities in @outDimTags@ and parent-child dictionaryAs @outDimTagDic@.
@outDimTagDic@ orders @objects@ and then @tools@, associating their child entities as a vector.
-}
gmshModelOccCut ::
  -- | objects
  S.Vector Entity ->
  -- | tools
  S.Vector Entity ->
  -- | tag
  EntityTag ->
  -- | removeObject
  Bool ->
  -- | removeTool
  Bool ->
  -- | @(outDimTags, outDimTagDic)@
  IO ((S.Vector Entity, V.Vector (S.Vector Entity)), StatusCode)
gmshModelOccCut
  (encodeEntities -> objects)
  (encodeEntities -> tools)
  (EntityTag tag)
  (bool -> removeObject)
  (bool -> removeTool) =
    withStatusCode $ \ierr -> fmap (parseEntities *** V.map S.unsafeCast) $
      withCVector $ \outDimTags outDimTags_n ->
        withNestedVector_ $ \outDimTagsMap outDimTagsMap_n outDimTagsMap_nn ->
          [C.exp| void {
            gmshModelOccCut
              ( $vec-ptr:(int * objects)
              , $vec-len:objects
              , $vec-ptr:(int * tools)
              , $vec-len:tools
              , $(int ** outDimTags)
              , $(size_t * outDimTags_n)
              , $(int *** outDimTagsMap)
              , $(size_t ** outDimTagsMap_n)
              , $(size_t * outDimTagsMap_nn)
              , $(const int tag)
              , $(const int removeObject)
              , $(const int removeTool)
              , $(int * ierr)
              )
            }|]

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

Return the resulting entities in @outDimTags@ and parent-child dictionaryAs @outDimTagDic@.
@outDimTagDic@ orders @objects@ and then @tools@, associating their child entities as a vector.
-}
gmshModelOccFragment ::
  -- | objects
  S.Vector Entity ->
  -- | tools
  S.Vector Entity ->
  -- | tag
  EntityTag ->
  -- | removeObject
  Bool ->
  -- | removeTool
  Bool ->
  -- | @(outDimTags, outDimTagDic)@
  IO ((S.Vector Entity, V.Vector (S.Vector Entity)), StatusCode)
gmshModelOccFragment
  (encodeEntities -> objects)
  (encodeEntities -> tools)
  (EntityTag tag)
  (bool -> removeObject)
  (bool -> removeTool) =
    withStatusCode $ \ierr -> fmap (parseEntities *** V.map S.unsafeCast) $
      withCVector $ \outDimTags outDimTags_n ->
        withNestedVector_ $ \outDimTagsMap outDimTagsMap_n outDimTagsMap_nn ->
          [C.exp| void {
            gmshModelOccFragment
              ( $vec-ptr:(int * objects)
              , $vec-len:objects
              , $vec-ptr:(int * tools)
              , $vec-len:tools
              , $(int ** outDimTags)
              , $(size_t * outDimTags_n)
              , $(int *** outDimTagsMap)
              , $(size_t ** outDimTagsMap_n)
              , $(size_t * outDimTagsMap_nn)
              , $(const int tag)
              , $(const int removeObject)
              , $(const int removeTool)
              , $(int * ierr)
              )
            }|]

-- | Copy the entities @entities@ in the OpenCASCADE CAD representation; the new entities are returned.
gmshModelOccCopy :: S.Vector Entity -> IO (S.Vector Entity, StatusCode)
gmshModelOccCopy (encodeEntities -> entities) =
  withStatusCode $ \ierr -> parseEntitiesM $
    withCVector_ $ \outDimTags outDimTags_n ->
      [C.exp| void {
        gmshModelOccCopy
          ( $vec-ptr:(int * entities)
          , $vec-len:entities
          , $(int ** outDimTags)
          , $(size_t *outDimTags_n)
          , $(int *ierr)
          )
      }|]

{- |
Get the OpenCASCADE entities in the bounding box defined by the two points
(@xmin@, @ymin@, @zmin@) and (@xmax@, @ymax@, @zmax@). If @dim@ is >= 0,
return only the entities of the specified dimension (e.g. points if @dim@
== 0).
-}
gmshModelOccGetEntitiesInBoundingBox ::
  -- | min
  Coord ->
  -- | max
  Coord ->
  Dimension ->
  IO (S.Vector Entity, StatusCode)
gmshModelOccGetEntitiesInBoundingBox
  (Coord (Vec3 xmin ymin zmin))
  (Coord (Vec3 xmax ymax zmax))
  (Dimension dim) =
    withStatusCode $ \ierr -> parseEntitiesM $
      withCVector_ $ \tags tags_n ->
        [C.exp|void {
          gmshModelOccGetEntitiesInBoundingBox
          ( $(const double xmin),
            $(const double ymin),
            $(const double zmin),
            $(const double xmax),
            $(const double ymax),
            $(const double zmax),
            $(int ** tags), $(size_t * tags_n),
            $(const int dim),
            $(int * ierr)
          )
        }|]

{- |
Get the bounding box (@xmin@, @ymin@, @zmin@), (@xmax@, @ymax@, @zmax@) of
the OpenCASCADE entity.
-}
gmshModelOccGetBoundingBox :: Entity -> IO ((Coord, Coord), StatusCode)
gmshModelOccGetBoundingBox (CEntity dim tag) =
  withStatusCode $ \ierr ->
    withPtrs @Coord $ \(xmin, ymin, zmin) ->
      withPtrs_ @Coord $ \(xmax, ymax, zmax) ->
        [C.exp| void {
        gmshModelOccGetBoundingBox
        ( $(const int dim)
        , $(const int tag)
        , $(double * xmin), $(double * ymin), $(double *zmin)
        , $(double * xmax), $(double * ymax), $(double *zmax)
        , $(int * ierr)
        )
      }|]

{- |
Get the center of mass of the OpenCASCADE entity.
-}
gmshModelOccGetCenterOfMass :: Entity -> IO (Coord, StatusCode)
gmshModelOccGetCenterOfMass (CEntity dim tag) =
  withStatusCode $ \ierr ->
    withPtrs_ @Coord $ \(x, y, z) ->
      [C.exp| void {
        gmshModelOccGetCenterOfMass
        ( $(const int dim)
        , $(const int tag)
        , $(double * x), $(double * y), $(double *z)
        , $(int * ierr)
        )
      }|]

{- |
Scale the entities @entities@ in the OpenCASCADE CAD representation by
factors @a@, @b@ and @c@ along the three coordinate axes; use (@x@, @y@,
@z@) as the center of the homothetic transformation.
-}
gmshModelOccDilate ::
  S.Vector Entity ->
  -- | @a@
  CDouble ->
  -- | @b@
  CDouble ->
  -- | @c@
  CDouble ->
  Coord ->
  IO StatusCode
gmshModelOccDilate (encodeEntities -> entities) a b c (Coord (Vec3 x y z)) =
  withStatusCode_ $ \ierr ->
    [C.exp|void {
      gmshModelOccDilate
        ( $vec-ptr:(int * entities),
          $vec-len:entities,
          $(const double x),
          $(const double y),
          $(const double z),
          $(const double a),
          $(const double b),
          $(const double c),
          $(int * ierr)
        )
    }|]

{- |
Get all the OpenCASCADE entities. If @dim@ is >= 0, return only the
entities of the specified dimension (e.g. points if @dim@ == 0). The
entities are returned as a vector of (dim, tag) integer pairs.
-}
gmshModelOccGetEntities ::
  Dimension -> IO (S.Vector Entity, StatusCode)
gmshModelOccGetEntities (Dimension dim) = withStatusCode $ \ierr ->
  parseEntitiesM $
    withCVector_ $ \dimTags dimTags_n ->
      [C.exp| void {
      gmshModelOccGetEntities
        ( $(int ** dimTags)
        , $(size_t * dimTags_n),
          $(const int dim),
          $(int * ierr)
        )
    }|]

gmshModelOccTranslate ::
  S.Vector Entity -> Vec3 -> IO StatusCode
gmshModelOccTranslate (encodeEntities -> dimTags) (Vec3 dx dy dz) =
  withStatusCode_ $ \ierr ->
    [C.exp|void {
      gmshModelOccTranslate
        ( $vec-ptr:(int * dimTags)
        , $vec-len:dimTags
        , $(const double dx)
        , $(const double dy)
        , $(const double dz)
        , $(int * ierr)
        )
    }|]

gmshModelOccMirror ::
  S.Vector Entity -> CDouble -> CDouble -> CDouble -> CDouble -> IO StatusCode
gmshModelOccMirror (encodeEntities -> dimTags) a b c d =
  withStatusCode_ $ \ierr ->
    [C.exp|void {
      gmshModelOccMirror
        ( $vec-ptr:(int * dimTags)
        , $vec-len:dimTags
        , $(const double a)
        , $(const double b)
        , $(const double c)
        , $(const double d)
        , $(int * ierr)
        )
    }|]

{- |
Add a parallelepipedic box in the OpenCASCADE CAD representation, defined
by a point (@x@, @y@, @z@) and the extents along the x-, y- and z-axes. If
@tag@ is positive, set the tag explicitly; otherwise a new tag is selected
automatically. Return the tag of the box.
-}
gmshModelOccAddBox ::
  Coord -> Vec3 -> EntityTag -> IO (EntityTag, StatusCode)
gmshModelOccAddBox (Coord (Vec3 x y z)) (Vec3 dx dy dz) (EntityTag tag) =
  withStatusCode $ \ierr ->
    EntityTag
      <$> [C.exp| int {
      gmshModelOccAddBox
        ( $(const double x), $(const double y), $(const double z)
        , $(const double dx), $(const double dy), $(const double dz)
        , $(const int tag)
        , $(int * ierr)
        )
    } |]

{- |
Remove the @entities@ in the OpenCASCADE CAD representation. If
@recursivep is true, remove all the entities on their boundaries, down to
dimension 0.
-}
gmshModelOccRemove ::
  S.Vector Entity ->
  -- | @recursive
  Bool ->
  IO StatusCode
gmshModelOccRemove (encodeEntities -> entities) (bool -> recursive) =
  withStatusCode_ $ \ierr ->
    [C.exp| void { 
      gmshModelOccRemove(
        $vec-ptr:(int * entities), $vec-len:entities,
        $(const int recursive),
        $(int * ierr)
      )
    }|]

{- |
Add a rectangle in the OpenCASCADE CAD representation, with lower left
corner at (@x@, @y@, @z@) and upper right corner at (@x@ + @dx@, @y@ +
@dy@, @z@). If @tag@ is positive, set the tag explicitly; otherwise a new
tag is selected automatically. Round the corners if @roundedRadius@ is
nonzero. Return the tag of the rectangle.
-}
gmshModelOccAddRectangle ::
  -- | @(x, y, z)@
  Coord ->
  -- | @(dx, dy)@
  Vec2 ->
  -- | @tag@
  EntityTag ->
  -- | @roundedRadius@
  CDouble ->
  IO (EntityTag, StatusCode)
gmshModelOccAddRectangle
  (Coord (Vec3 x y z))
  (Vec2 dx dy)
  (EntityTag tag)
  roundedRadius =
    withStatusCode $ \ierr ->
      EntityTag
        <$> [C.exp| int {
              gmshModelOccAddRectangle
                ( $(const double x),
                  $(const double y),
                  $(const double z),
                  $(const double dx),
                  $(const double dy),
                  $(const int tag),
                  $(const double roundedRadius),
                  $(int * ierr))
            }|]

{- |
Add a surface in the OpenCASCADE CAD representation, filling the curve loop
@wireTag@. If @tag@ is positive, set the tag explicitly; otherwise a new
tag is selected automatically. Return the tag of the surface. If
@pointTags@ are provided, force the surface to pass through the given
points.
-}
gmshModelOccAddSurfaceFilling ::
  -- | @wireTag@
  EntityTag ->
  -- | @tag@
  EntityTag ->
  -- | @pointTags@
  S.Vector EntityTag ->
  IO (EntityTag, StatusCode)
gmshModelOccAddSurfaceFilling (EntityTag wireTag) (EntityTag tag) (S.unsafeCast -> pointTags) =
  withStatusCode $ \ierr ->
    EntityTag
      <$> [C.exp|int {
      gmshModelOccAddSurfaceFilling
        ( $(const int wireTag),
          $(const int tag),
          $vec-ptr:(int * pointTags), $vec-len:pointTags,
          $(int * ierr)
        )
    }|]

{- |
Add a new mesh size field of type @fieldType@. If @tag@ is positive, assign
the tag explicitly; otherwise a new tag is assigned automatically. Return
the field tag.
-}
gmshModelMeshFieldAdd ::
  BS.ByteString -> MeshFieldTag -> IO (MeshFieldTag, StatusCode)
gmshModelMeshFieldAdd fieldType (MeshFieldTag tag) =
  withStatusCode $ \ierr ->
    MeshFieldTag
      <$> [C.exp| int {
        gmshModelMeshFieldAdd($bs-cstr:fieldType, $(const int tag), $(int * ierr))
      }|]

-- | Remove the field with tag @tag@.
gmshModelMeshFieldRemove :: MeshFieldTag -> IO StatusCode
gmshModelMeshFieldRemove (MeshFieldTag tag) =
  withStatusCode_ $ \ierr ->
    [C.exp| void {
      gmshModelMeshFieldRemove($(const int tag), $(int * ierr))
    }|]

-- | Set the numerical option @option@ to value @value@ for field @tag@.
gmshModelMeshFieldSetNumber ::
  MeshFieldTag -> BS.ByteString -> CDouble -> IO StatusCode
gmshModelMeshFieldSetNumber (MeshFieldTag tag) option value =
  withStatusCode_ $ \ierr ->
    [C.exp| void {
      gmshModelMeshFieldSetNumber
        ( $(const int tag)
        , $bs-cstr:option
        , $(const double value)
        , $(int * ierr)
        )
    }|]

-- | Set the string option @option@ to value @value@ for field @tag@.
gmshModelMeshFieldSetString ::
  MeshFieldTag -> BS.ByteString -> BS.ByteString -> IO StatusCode
gmshModelMeshFieldSetString (MeshFieldTag tag) option value =
  withStatusCode_ $ \ierr ->
    [C.exp| void {
      gmshModelMeshFieldSetString
        ( $(const int tag)
        , $bs-cstr:option
        , $bs-cstr:value
        , $(int * ierr)
        )
    }|]

-- | Set the numerical list option @option@ to value @value@ for field @tag@.
gmshModelMeshFieldSetNumbers ::
  MeshFieldTag -> BS.ByteString -> S.Vector CDouble -> IO StatusCode
gmshModelMeshFieldSetNumbers (MeshFieldTag tag) option value =
  withStatusCode_ $ \ierr ->
    [C.exp| void {
      gmshModelMeshFieldSetNumbers
        ( $(const int tag)
        , $bs-cstr:option
        , $vec-ptr:(double *value)
        , $vec-len:value
        , $(int * ierr)
        )
    }|]

-- | Set the field @tag@ as the background mesh size field.
gmshModelMeshFieldSetAsBackgroundMesh :: MeshFieldTag -> IO StatusCode
gmshModelMeshFieldSetAsBackgroundMesh (MeshFieldTag tag) =
  withStatusCode_ $ \ierr ->
    [C.exp| void {
      gmshModelMeshFieldSetAsBackgroundMesh($(const int tag), $(int * ierr))
    }|]

-- | Set the field @tag@ as the boundary layer size field.
gmshModelMeshFieldSetAsBoundaryLayer :: MeshFieldTag -> IO StatusCode
gmshModelMeshFieldSetAsBoundaryLayer (MeshFieldTag tag) =
  withStatusCode_ $ \ierr ->
    [C.exp| void {
      gmshModelMeshFieldSetAsBoundaryLayer($(const int tag), $(int * ierr))
    }|]

gmshFltkInitialize :: IO StatusCode
gmshFltkInitialize = withStatusCode_ $ \ierr ->
  [C.exp| void{ 
    gmshFltkInitialize($(int * ierr))
  }|]

gmshFltkWait :: CDouble -> IO StatusCode
gmshFltkWait time = withStatusCode_ $ \ierr ->
  [C.exp| void{ 
    gmshFltkWait($(const double time), $(int * ierr))
  }|]

gmshFltkUpdate :: IO StatusCode
gmshFltkUpdate = withStatusCode_ $ \ierr ->
  [C.exp| void{ 
    gmshFltkUpdate($(int * ierr))
  }|]

gmshFltkRun :: IO StatusCode
gmshFltkRun = withStatusCode_ $ \ierr ->
  [C.exp| void{ 
    gmshFltkRun($(int * ierr))
  }|]
