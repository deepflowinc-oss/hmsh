{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Numeric.Mesh.Gmsh.Model.Mesh
  ( setOrder,
    recombine,
    setTransfiniteVolume,
    TransVolumeCorners (..),
    TransfiniteMeshing (..),
    CurveTransfiniteMethod (..),
    renderTransfiniteCurveMethod,
    setTransfiniteCurve,
    TransSurfaceArrangement (..),
    setTransfiniteSurface,
    TransSurfaceCorners (..),
    generateMesh,
    NodeOpts (..),
    NodeInfo (..),
    getNodes,
    removeDuplicateNodes,
  )
where

import Control.Arrow ((&&&))
import Control.DeepSeq (NFData)
import qualified Control.Foldl as L
import Control.Monad.IO.Class (MonadIO)
import Data.Coerce (coerce)
import Data.Functor.Compose (Compose (Compose))
import Data.Hashable
import Data.Maybe (isJust, isNothing)
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import Foreign.C.Types (CInt)
import GHC.Generics (Generic)
import qualified Numeric.Mesh.Gmsh.LowLevel as Low
import Numeric.Mesh.Gmsh.Types
import UnliftIO.Exception (throwIO)

setOrder :: MonadIO m => CInt -> GmshT m ()
setOrder = embedLow_ . Low.gmshModelMeshSetOrder

recombine :: MonadIO m => GmshT m ()
recombine = embedLow_ Low.gmshModelMeshRecombine

data TransVolumeCorners a
  = HexaCorners !a !a !a !a !a !a
  | OctaCorners !a !a !a !a !a !a !a !a
  deriving (Functor, Traversable, Foldable, Show, Eq, Ord)
  deriving (Generic, Typeable)
  deriving anyclass (Hashable, NFData)

{- | Set transfinite constraint on Meshing side;
Note that the constraint will be CLEARED after 'synchronize'.
To persist them, use corresponding function in "Numeric.Mesh.Gmsh.Model.Geo".
-}
setTransfiniteVolume ::
  (MonadIO m) =>
  Volume ->
  Maybe (TransVolumeCorners Point) ->
  GmshT m ()
setTransfiniteVolume (Entity vol) points =
  embedLow_ $
    Low.gmshModelMeshSetTransfiniteVolume vol $
      L.fold (L.premap rawEntityTag L.vector) (Compose points)

{- | Set transfinite constraint on Meshing side;
Note that the constraint will be CLEARED after 'synchronize'.
To persist them, use corresponding function in "Numeric.Mesh.Gmsh.Model.Geo".
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
    Low.gmshModelMeshSetTransfiniteCurve (coerce curve) (fromIntegral numNodes) (renderTransfiniteCurveMethod method) (realToFrac coeff)

{- | Entities that with transfinite meshing constraint.

Note that the constraint will be CLEARED after @synchronize@.
To persist them, use corresponding function in "Numeric.Mesh.Gmsh.Model.Geo".
-}
class HasDimension e => TransfiniteMeshing e where
  data TransfiniteConfig e
  setTransfinite ::
    MonadIO m =>
    Entity e ->
    TransfiniteConfig e ->
    GmshT m ()

instance TransfiniteMeshing 'VolumeE where
  newtype TransfiniteConfig VolumeE = TransVolumeCornerPoints {getCornerPoints :: Maybe (TransVolumeCorners Point)}
    deriving (Show, Eq, Ord, Generic, Typeable)
    deriving anyclass (NFData)
  setTransfinite vol = setTransfiniteVolume vol . getCornerPoints

instance TransfiniteMeshing 'CurveE where
  data TransfiniteConfig CurveE = TransCurveConfig
    { nodeCount :: !Int
    , method :: !CurveTransfiniteMethod
    , coefficient :: !Double
    }
  setTransfinite curve TransCurveConfig {..} =
    setTransfiniteCurve curve nodeCount method coefficient

setTransfiniteSurface ::
  (Foldable t, MonadIO m) =>
  Surface ->
  TransSurfaceArrangement ->
  t Point ->
  GmshT m ()
setTransfiniteSurface surf arrang corners =
  embedLow_ $
    Low.gmshModelMeshSetTransfiniteSurface (coerce surf) (renderTransSurfaceArrangement arrang) $
      L.fold (L.premap rawEntityTag L.vector) corners

instance TransfiniteMeshing 'SurfaceE where
  data TransfiniteConfig 'SurfaceE = TransSurfaceConfig
    { surfaceArrangement :: TransSurfaceArrangement
    , surfaceCornerPoitns :: Maybe (TransSurfaceCorners Point)
    }
  setTransfinite surf TransSurfaceConfig {..} =
    setTransfiniteSurface
      surf
      surfaceArrangement
      (Compose surfaceCornerPoitns)

generateMesh :: MonadIO m => Dimension -> GmshT m ()
generateMesh = embedLow_ . Low.gmshModelMeshGenerate

data NodeOpts entity = NodeOpts
  { ndDimension :: Maybe Dimension
  , ndTag :: Maybe (EntityTag entity)
  , ndIncludeBoundary :: Bool
  , ndReturnParametricCoord :: Bool
  }
  deriving (Show, Eq, Ord, Generic, Typeable)

data NodeInfo entity = NodeInfo
  { nodeTag :: NodeTag
  , nodeCoord :: Coord
  , nodeParamCoord :: Maybe (ParametricCoord entity)
  }
  deriving (Show, Eq, Ord, Typeable, Generic)

-- | Simple version of 'getNodes'', just returning node tags and coords without boundary.
getNodes ::
  (MonadIO m, HasDimension entity) =>
  Entity entity ->
  GmshT m (V.Vector (NodeTag, Coord))
getNodes ent = do
  infos <-
    getNodes'
      NodeOpts
        { ndDimension = Just $ entityDim ent
        , ndTag = Just $ entityTag ent
        , ndIncludeBoundary = False
        , ndReturnParametricCoord = False
        }
  pure $ V.map (nodeTag &&& nodeCoord) infos

getNodes' ::
  (MonadIO m, HasDimension entity) =>
  NodeOpts entity ->
  GmshT m (V.Vector (NodeInfo entity))
getNodes' NodeOpts {..}
  | isNothing ndDimension && isJust ndTag =
    throwIO $ ValidationError "getNodes: Tag is specified, but no dimension specified!"
  | otherwise = do
    (nodeTags, coords, paramCoords) <-
      embedLow $
        Low.gmshModelMeshGetNodes
          ( Low.Entity
              (maybe (-1) coerce ndDimension)
              (maybe (-1) coerce ndTag)
          )
          ndIncludeBoundary
          ndReturnParametricCoord
    let pCs
          | S.null paramCoords = Nothing
          | otherwise = Just $ V.map (mapVec realToFrac) $ V.convert $ parseElems paramCoords
    pure $
      V.izipWith
        ( \i nodeTag nodeCoord ->
            let nodeParamCoord = (V.! i) <$> pCs
             in NodeInfo {..}
        )
        (V.convert nodeTags)
        (V.convert coords)

removeDuplicateNodes :: MonadIO m => GmshT m ()
removeDuplicateNodes = embedLow_ Low.gmshModelMeshRemoveDuplicateNodes
