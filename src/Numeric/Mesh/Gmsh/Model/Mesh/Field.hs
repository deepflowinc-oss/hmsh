{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Numeric.Mesh.Gmsh.Model.Mesh.Field
  ( setFieldOption,
    addSizeFieldWith,
    addSizeField,
    addBoundaryLayerWith,
    addBoundaryLayer,
    removeSizeField,
    setAsBoundaryLayer,
    setAsBackgroundMesh,
    SizeFieldType (..),
    SSizeFieldType (..),
    SizeFieldOption (..),
    SizeField (..),
    KnownSizeFieldType (..),
    FieldOptionValue (..),
    Attractor,
    AttractorAnisoCurve,
    AutomaticMeshSizeField,
    Ball,
    BoundaryLayer,
    Box,
    Curvature,
    Cylinder,
    Distance,
    ExternalProcess,
    Frustum,
    Gradient,
    IntersectAniso,
    Laplacian,
    LonLat,
    MathEval,
    MathEvalAniso,
    Max,
    MaxEigenHessian,
    Mean,
    Min,
    MinAniso,
    Octree,
    Param,
    PostView,
    Restrict,
    Structured,
    Threshold,
    IsSizeFieldOptionValue (..),
    OneOf,
    HasPointsList,
    HasCurvesList,
    HasSurfacesList,
    HasDistBound,
    HasRadius,
    HasThickness,
    HasInOutValues,
    HasCenterCoord,
    HasDelta,
    HasInField,
    HasFieldsList,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Coerce (coerce)
import Data.Kind (Constraint)
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as S
import Foreign.C (CDouble, CInt)
import GHC.Generics (Generic)
import GHC.TypeLits
import Numeric.Mesh.Gmsh
import Numeric.Mesh.Gmsh.LowLevel
  ( gmshModelMeshFieldAdd,
    gmshModelMeshFieldRemove,
    gmshModelMeshFieldSetAsBackgroundMesh,
    gmshModelMeshFieldSetAsBoundaryLayer,
    gmshModelMeshFieldSetNumber,
    gmshModelMeshFieldSetNumbers,
    gmshModelMeshFieldSetString,
  )
import Numeric.Mesh.Gmsh.LowLevel.Types hiding (Entity)
import Type.Reflection (Typeable)

addSizeFieldWith ::
  forall field m.
  (KnownSizeFieldType field, MonadIO m) =>
  MeshFieldTag ->
  GmshT m (SizeField field)
addSizeFieldWith tag =
  fmap SizeField $
    embedLow $
      gmshModelMeshFieldAdd (sizeFieldLabel @field) tag

addSizeField ::
  forall field m.
  (KnownSizeFieldType field, MonadIO m) =>
  GmshT m (SizeField field)
addSizeField =
  fmap SizeField $
    embedLow $
      gmshModelMeshFieldAdd (sizeFieldLabel @field) (-1)

addBoundaryLayerWith ::
  (MonadIO m) =>
  MeshFieldTag ->
  GmshT m (SizeField BoundaryLayer)
addBoundaryLayerWith = addSizeFieldWith

addBoundaryLayer :: (MonadIO m) => GmshT m (SizeField BoundaryLayer)
addBoundaryLayer = addSizeField

removeSizeField :: MonadIO m => SizeField f -> GmshT m ()
removeSizeField (SizeField f) = embedLow_ $ gmshModelMeshFieldRemove f

setAsBoundaryLayer :: MonadIO m => SizeField BoundaryLayer -> GmshT m ()
setAsBoundaryLayer (SizeField tag) =
  embedLow_ $ gmshModelMeshFieldSetAsBoundaryLayer tag

setAsBackgroundMesh :: MonadIO m => SizeField BoundaryLayer -> GmshT m ()
setAsBackgroundMesh (SizeField tag) =
  embedLow_ $ gmshModelMeshFieldSetAsBackgroundMesh tag

data SizeFieldType
  = Attractor
  | AttractorAnisoCurve
  | AutomaticMeshSizeField
  | Ball
  | BoundaryLayer
  | Box
  | Curvature
  | Cylinder
  | Distance
  | ExternalProcess
  | Frustum
  | Gradient
  | IntersectAniso
  | Laplacian
  | LonLat
  | MathEval
  | MathEvalAniso
  | Max
  | MaxEigenHessian
  | Mean
  | Min
  | MinAniso
  | Octree
  | Param
  | PostView
  | Restrict
  | Structured
  | Threshold
  deriving (Show, Eq, Ord, Typeable, Generic, Enum)

data SSizeFieldType a where
  SAttractor :: SSizeFieldType 'Attractor
  SAttractorAnisoCurve :: SSizeFieldType 'AttractorAnisoCurve
  SAutomaticMeshSizeField :: SSizeFieldType 'AutomaticMeshSizeField
  SBall :: SSizeFieldType 'Ball
  SBoundaryLayer :: SSizeFieldType 'BoundaryLayer
  SBox :: SSizeFieldType 'Box
  SCurvature :: SSizeFieldType 'Curvature
  SCylinder :: SSizeFieldType 'Cylinder
  SDistance :: SSizeFieldType 'Distance
  SExternalProcess :: SSizeFieldType 'ExternalProcess
  SFrustum :: SSizeFieldType 'Frustum
  SGradient :: SSizeFieldType 'Gradient
  SIntersectAniso :: SSizeFieldType 'IntersectAniso
  SLaplacian :: SSizeFieldType 'Laplacian
  SLonLat :: SSizeFieldType 'LonLat
  SMathEval :: SSizeFieldType 'MathEval
  SMathEvalAniso :: SSizeFieldType 'MathEvalAniso
  SMax :: SSizeFieldType 'Max
  SMaxEigenHessian :: SSizeFieldType 'MaxEigenHessian
  SMean :: SSizeFieldType 'Mean
  SMin :: SSizeFieldType 'Min
  SMinAniso :: SSizeFieldType 'MinAniso
  SOctree :: SSizeFieldType 'Octree
  SParam :: SSizeFieldType 'Param
  SPostView :: SSizeFieldType 'PostView
  SRestrict :: SSizeFieldType 'Restrict
  SStructured :: SSizeFieldType 'Structured
  SThreshold :: SSizeFieldType 'Threshold

class KnownSizeFieldType (fld :: SizeFieldType) where
  sSizeFieldType :: SSizeFieldType fld
  sizeFieldLabel :: ByteString

instance KnownSizeFieldType 'Attractor where
  sSizeFieldType = SAttractor
  sizeFieldLabel = "Attractor"

type Attractor = 'Attractor

instance KnownSizeFieldType 'AttractorAnisoCurve where
  sSizeFieldType = SAttractorAnisoCurve
  sizeFieldLabel = "AttractorAnisoCurve"

type AttractorAnisoCurve = 'AttractorAnisoCurve

instance KnownSizeFieldType 'AutomaticMeshSizeField where
  sSizeFieldType = SAutomaticMeshSizeField
  sizeFieldLabel = "AutomaticMeshSizeField"

type AutomaticMeshSizeField = 'AutomaticMeshSizeField

instance KnownSizeFieldType 'Ball where
  sSizeFieldType = SBall
  sizeFieldLabel = "Ball"

type Ball = 'Ball

instance KnownSizeFieldType 'BoundaryLayer where
  sSizeFieldType = SBoundaryLayer
  sizeFieldLabel = "BoundaryLayer"

type BoundaryLayer = 'BoundaryLayer

instance KnownSizeFieldType 'Box where
  sSizeFieldType = SBox
  sizeFieldLabel = "Box"

type Box = 'Box

instance KnownSizeFieldType 'Curvature where
  sSizeFieldType = SCurvature
  sizeFieldLabel = "Curvature"

type Curvature = 'Curvature

instance KnownSizeFieldType 'Cylinder where
  sSizeFieldType = SCylinder
  sizeFieldLabel = "Cylinder"

type Cylinder = 'Cylinder

instance KnownSizeFieldType 'Distance where
  sSizeFieldType = SDistance
  sizeFieldLabel = "Distance"

type Distance = 'Distance

instance KnownSizeFieldType 'ExternalProcess where
  sSizeFieldType = SExternalProcess
  sizeFieldLabel = "ExternalProcess"

type ExternalProcess = 'ExternalProcess

instance KnownSizeFieldType 'Frustum where
  sSizeFieldType = SFrustum
  sizeFieldLabel = "Frustum"

type Frustum = 'Frustum

instance KnownSizeFieldType 'Gradient where
  sSizeFieldType = SGradient
  sizeFieldLabel = "Gradient"

type Gradient = 'Gradient

instance KnownSizeFieldType 'IntersectAniso where
  sSizeFieldType = SIntersectAniso
  sizeFieldLabel = "IntersectAniso"

type IntersectAniso = 'IntersectAniso

instance KnownSizeFieldType 'Laplacian where
  sSizeFieldType = SLaplacian
  sizeFieldLabel = "Laplacian"

type Laplacian = 'Laplacian

instance KnownSizeFieldType 'LonLat where
  sSizeFieldType = SLonLat
  sizeFieldLabel = "LonLat"

type LonLat = 'LonLat

instance KnownSizeFieldType 'MathEval where
  sSizeFieldType = SMathEval
  sizeFieldLabel = "MathEval"

type MathEval = 'MathEval

instance KnownSizeFieldType 'MathEvalAniso where
  sSizeFieldType = SMathEvalAniso
  sizeFieldLabel = "MathEvalAniso"

type MathEvalAniso = 'MathEvalAniso

instance KnownSizeFieldType 'Max where
  sSizeFieldType = SMax
  sizeFieldLabel = "Max"

type Max = 'Max

instance KnownSizeFieldType 'MaxEigenHessian where
  sSizeFieldType = SMaxEigenHessian
  sizeFieldLabel = "MaxEigenHessian"

type MaxEigenHessian = 'MaxEigenHessian

instance KnownSizeFieldType 'Mean where
  sSizeFieldType = SMean
  sizeFieldLabel = "Mean"

type Mean = 'Mean

instance KnownSizeFieldType 'Min where
  sSizeFieldType = SMin
  sizeFieldLabel = "Min"

type Min = 'Min

instance KnownSizeFieldType 'MinAniso where
  sSizeFieldType = SMinAniso
  sizeFieldLabel = "MinAniso"

type MinAniso = 'MinAniso

instance KnownSizeFieldType 'Octree where
  sSizeFieldType = SOctree
  sizeFieldLabel = "Octree"

type Octree = 'Octree

instance KnownSizeFieldType 'Param where
  sSizeFieldType = SParam
  sizeFieldLabel = "Param"

type Param = 'Param

instance KnownSizeFieldType 'PostView where
  sSizeFieldType = SPostView
  sizeFieldLabel = "PostView"

type PostView = 'PostView

instance KnownSizeFieldType 'Restrict where
  sSizeFieldType = SRestrict
  sizeFieldLabel = "Restrict"

type Restrict = 'Restrict

instance KnownSizeFieldType 'Structured where
  sSizeFieldType = SStructured
  sizeFieldLabel = "Structured"

type Structured = 'Structured

instance KnownSizeFieldType 'Threshold where
  sSizeFieldType = SThreshold
  sizeFieldLabel = "Threshold"

type Threshold = 'Threshold

newtype SizeField (fld :: SizeFieldType) = SizeField {runSizeField :: MeshFieldTag}

type OneOf (xs :: [SizeFieldType]) (x :: SizeFieldType) =
  OneOf' ( 'Text "Must be one of " ':<>: 'ShowType xs ':<>: 'Text ", but got: " ':<>: 'ShowType x) xs x

type family OneOf' err (xs :: [SizeFieldType]) x :: Constraint where
  OneOf' err '[] _ = TypeError err
  OneOf' _ (x ': xs) x = ()
  OneOf' err (_ ': xs) x = OneOf' err xs x

type HasPointsList fld =
  OneOf '[Attractor, BoundaryLayer, Distance, Restrict] fld

type HasCurvesList fld =
  OneOf '[Attractor, AttractorAnisoCurve, BoundaryLayer, Distance, Restrict] fld

type HasSurfacesList fld =
  OneOf '[Attractor, Distance, Restrict] fld

type HasDistBound fld =
  OneOf '[AttractorAnisoCurve, Threshold] fld

type HasRadius fld = OneOf '[Ball, Cylinder] fld

type HasThickness fld = OneOf '[Ball, BoundaryLayer, Box] fld

type HasInOutValues fld = OneOf '[Ball, Box, Cylinder] fld

type HasCenterCoord fld = OneOf '[Ball, Cylinder] fld

type HasDelta fld =
  OneOf
    '[Curvature, Gradient, Laplacian, MaxEigenHessian, Mean]
    fld

type HasInField fld =
  OneOf
    '[ Curvature
     , Gradient
     , Laplacian
     , LonLat
     , MaxEigenHessian
     , Mean
     , Octree
     , Param
     , Restrict
     , Threshold
     ]
    fld

type HasFieldsList fld =
  OneOf '[IntersectAniso, Max, Min, MinAniso] fld

data SizeFieldOption fld a where
  FieldX, FieldY, FieldZ :: OneOf '[Attractor, Distance] fld => SizeFieldOption fld (SizeField ty)
  NumPointsPerCurve ::
    OneOf '[Attractor, AttractorAnisoCurve, Distance] fld =>
    SizeFieldOption fld Int
  PointsList :: HasPointsList fld => SizeFieldOption fld [Point]
  CurvesList :: HasCurvesList fld => SizeFieldOption fld [Curve]
  SurfacesList :: HasSurfacesList fld => SizeFieldOption fld [Surface]
  VolumesList :: SizeFieldOption 'Restrict [Volume]
  DistMax, DistMin :: HasDistBound fld => SizeFieldOption fld Double
  SizeMaxNormal, SizeMaxTangent, SizeMinNormal, SizeMinTangent :: SizeFieldOption AttractorAnisoCurve Double
  Opt_features :: SizeFieldOption AutomaticMeshSizeField Bool
  Opt_gradation :: SizeFieldOption AutomaticMeshSizeField Double
  Opt_hBulk :: SizeFieldOption AutomaticMeshSizeField Double
  Opt_hMax :: SizeFieldOption AutomaticMeshSizeField Double
  Opt_hMin :: SizeFieldOption AutomaticMeshSizeField Double
  Opt_nPointsPerCircle :: SizeFieldOption AutomaticMeshSizeField Int
  Opt_nPointsPerGap :: SizeFieldOption AutomaticMeshSizeField Int
  Opt_p4estFileToLoad :: SizeFieldOption AutomaticMeshSizeField ByteString
  Opt_smoothing :: SizeFieldOption AutomaticMeshSizeField Bool
  Radius :: HasRadius fld => SizeFieldOption fld Double
  Thickness :: HasThickness fld => SizeFieldOption fld Double
  VIn, VOut :: HasInOutValues fld => SizeFieldOption fld Double
  XCenter, YCenter, ZCenter :: HasCenterCoord fld => SizeFieldOption fld Double
  AnisoMax, Beta, BetaLaw :: SizeFieldOption BoundaryLayer Double
  ExcludedSurfacesList :: SizeFieldOption BoundaryLayer [Surface]
  FanPointsList :: SizeFieldOption BoundaryLayer [Point]
  FanPointsSizesList :: SizeFieldOption BoundaryLayer [Int]
  IntersectMetrics, NbLayers :: SizeFieldOption BoundaryLayer Int
  Quads :: SizeFieldOption BoundaryLayer Bool
  Ratio, Size, SizeFar :: SizeFieldOption BoundaryLayer Double
  SizesList :: SizeFieldOption BoundaryLayer [Double]
  XMax, XMin, YMax, YMin, ZMax, ZMin :: SizeFieldOption Box Double
  Delta :: HasDelta fld => SizeFieldOption fld Double
  InField :: HasInField fld => SizeFieldOption fld (SizeField f)
  XAxis, YAxis, ZAxis :: SizeFieldOption 'Cylinder Double
  CommandLine :: SizeFieldOption 'ExternalProcess ByteString
  InnerR1, InnerR2, InnerV1, InnerV2 :: SizeFieldOption 'Frustum Double
  OuterR1, OuterR2, OuterV1, OuterV2 :: SizeFieldOption 'Frustum Double
  X1, X2, Y1, Y2, Z1, Z2 :: SizeFieldOption 'Frustum Double
  Kind :: SizeFieldOption 'Gradient XYZ
  FieldsList :: HasFieldsList fld => SizeFieldOption fld [SizeField a]
  FromStereo :: SizeFieldOption 'LonLat Bool
  RadiusStereo :: SizeFieldOption 'LonLat Double
  F :: SizeFieldOption 'MathEval ByteString
  M11, M12, M13, M22, M23, M33 :: SizeFieldOption 'MathEvalAniso ByteString
  FX, FY, FZ :: SizeFieldOption 'Param ByteString
  CropNegativeValues :: SizeFieldOption 'PostView Bool
  ViewIndex :: SizeFieldOption 'PostView Int
  ViewTag :: SizeFieldOption 'PostView Int
  FileName :: SizeFieldOption 'Structured ByteString
  OutsideValue :: SizeFieldOption 'Structured Double
  SetOutsideValue :: SizeFieldOption 'Structured Bool
  TextFormat :: SizeFieldOption 'Structured Bool
  Sigmoid :: SizeFieldOption 'Threshold Bool
  SizeMax, SizeMin :: SizeFieldOption 'Threshold Double
  StopAtDistMax :: SizeFieldOption 'Threshold Bool
  OtherOption :: ByteString -> SizeFieldOption fld a

data XYZ = X | Y | Z
  deriving (Show, Eq, Ord, Typeable, Generic)

deriving instance Show (SizeFieldOption g f)

deriving instance Eq (SizeFieldOption g f)

deriving instance Ord (SizeFieldOption g f)

deriving instance Typeable SizeFieldOption

setFieldOption ::
  (IsSizeFieldOptionValue a, MonadIO m) =>
  SizeField f ->
  SizeFieldOption f a ->
  a ->
  GmshT m ()
setFieldOption (SizeField fld) optName a =
  embedLow_ $
    case toFieldOptionValue a of
      NumberVal cd -> gmshModelMeshFieldSetNumber fld (toFieldOptionName optName) cd
      StringVal bs -> gmshModelMeshFieldSetString fld (toFieldOptionName optName) bs
      NumbersVal nums -> gmshModelMeshFieldSetNumbers fld (toFieldOptionName optName) nums

toFieldOptionName :: SizeFieldOption f a -> ByteString
toFieldOptionName (OtherOption bs) = bs
toFieldOptionName opt =
  let orig = BS8.pack $ show opt
   in fromMaybe orig $ BS8.stripPrefix "Opt_" orig

data FieldOptionValue
  = NumberVal CDouble
  | StringVal ByteString
  | NumbersVal (S.Vector CDouble)
  deriving (Show, Eq, Ord, Typeable, Generic)

class IsSizeFieldOptionValue a where
  toFieldOptionValue :: a -> FieldOptionValue

instance IsSizeFieldOptionValue Int where
  toFieldOptionValue = NumberVal . realToFrac
  {-# INLINE toFieldOptionValue #-}

instance IsSizeFieldOptionValue XYZ where
  toFieldOptionValue X = NumberVal 1
  toFieldOptionValue Y = NumberVal 2
  toFieldOptionValue Z = NumberVal 3

instance IsSizeFieldOptionValue Bool where
  toFieldOptionValue =
    NumberVal . \case True -> 1; False -> 0
  {-# INLINE toFieldOptionValue #-}

instance IsSizeFieldOptionValue Double where
  toFieldOptionValue = NumberVal . realToFrac
  {-# INLINE toFieldOptionValue #-}

instance IsSizeFieldOptionValue ByteString where
  toFieldOptionValue = StringVal
  {-# INLINE toFieldOptionValue #-}

instance IsSizeFieldOptionValue [Entity a] where
  toFieldOptionValue = NumbersVal . S.fromList . map (realToFrac . coerce @_ @CInt)
  {-# INLINE toFieldOptionValue #-}

instance IsSizeFieldOptionValue [SizeField a] where
  toFieldOptionValue = NumbersVal . S.fromList . map (realToFrac . coerce @_ @CInt)
  {-# INLINE toFieldOptionValue #-}

instance IsSizeFieldOptionValue (Entity a) where
  toFieldOptionValue = NumberVal . realToFrac . coerce @_ @CInt
  {-# INLINE toFieldOptionValue #-}

instance IsSizeFieldOptionValue (SizeField a) where
  toFieldOptionValue = NumberVal . realToFrac . coerce @_ @CInt
  {-# INLINE toFieldOptionValue #-}
