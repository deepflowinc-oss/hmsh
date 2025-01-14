{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Numeric.Mesh.Gmsh.Types
  ( module Numeric.Mesh.Gmsh.Types,
    StatusCode (..),
    Vec2 (..),
    Vec3 (..),
    Vec (..),
    IsVec (..),
    parseVec,
    mapVec,
    parseElemsM,
    encodeElemsM,
    Coord (..),
    Dimension (..),
    PhysicalName (..),
    NodeTag (..),
  )
where

import Barbies
import Control.DeepSeq (NFData)
import Control.Monad (forM, void, when)
import Control.Monad.Identity (IdentityT (..))
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Accum
import Control.Monad.Writer.Class (MonadWriter)
import qualified Data.ByteString.Char8 as BS
import Data.Coerce (coerce)
import qualified Data.DList as DL
import Data.Functor ((<&>))
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Hashable (Hashable)
import Data.Monoid (Sum (Sum))
import Data.Ord (comparing)
import Data.Proxy (Proxy (Proxy))
import Data.Semigroup.Generic (GenericSemigroupMonoid (GenericSemigroupMonoid))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Type.Equality (TestEquality (testEquality), type (:~:) (Refl), type (~~))
import qualified Data.Vector as V
import Data.Vector.Unboxed.Deriving
import Foreign.C (CInt)
import Foreign.Storable
import GHC.Generics (Generic)
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:<>:)), TypeError)
import GHC.TypeNats (KnownNat, Nat, natVal, type (<=))
import Numeric.Mesh.Gmsh.LowLevel (Dimension (..), encodeElemsM, parseVec)
import qualified Numeric.Mesh.Gmsh.LowLevel as Low
import Numeric.Mesh.Gmsh.LowLevel.Types (Coord (..), IsVec (..), NodeTag (..), PhysicalName (..), StatusCode (..), Vec (..), Vec2 (..), Vec3 (..), mapVec, parseElemsM)
import UnliftIO

data GmshError
  = GmshError
      { statusCode :: StatusCode
      , errorMessage :: String
      }
  | ValidationError {errorMessage :: String}
  | PatternMatchFailure String
  deriving (Typeable, Show, Eq, Ord)
  deriving anyclass (Exception)

type Gmsh = GmshT IO

instance MonadIO m => MonadFail (GmshT m) where
  fail = throwIO . PatternMatchFailure

newtype GmshT m a = Gmsh {unGmsh :: m a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    )
  deriving (MonadTrans) via IdentityT

deriving newtype instance
  MonadReader env m =>
  MonadReader env (GmshT m)

deriving newtype instance
  MonadState s m =>
  MonadState s (GmshT m)

deriving newtype instance
  MonadWriter w m =>
  MonadWriter w (GmshT m)

data GmshOpts = GmshOpts
  { gmshCLIOptions :: [String]
  , gmshReadConfig :: Bool
  }
  deriving (Read, Show, Eq, Ord, Generic, Typeable)

defaultGmshOpts :: GmshOpts
defaultGmshOpts = GmshOpts [] False

{- |
Runs Gmsh. Automatically calls the Gmsh initializer and finalizer.

See also: 'runGmshWith_', 'runGmsh' and 'runGmsh_'
-}
runGmshWith ::
  MonadUnliftIO m =>
  GmshOpts ->
  GmshT m a ->
  m (Either GmshError a)
runGmshWith opts = try . runGmshWith_ opts

{- |
A variant of 'runGmshWith' which throws Gmsh-related exception.

See also: 'runGmshWith', 'runGmsh', and 'runGmsh_'
-}
runGmshWith_ :: MonadUnliftIO m => GmshOpts -> GmshT m a -> m a
runGmshWith_ GmshOpts {..} (Gmsh act) =
  bracket_
    ( liftIO $ do
        void $
          Low.gmshInitialize
            (map (T.encodeUtf8 . T.pack) gmshCLIOptions)
            gmshReadConfig
        Low.gmshLoggerStart
    )
    (liftIO $ Low.gmshLoggerStop *> Low.gmshFinalize)
    act

{- |
@runGmsh = 'runGmshWith' 'defaultGmshOpts'@.

See also: 'runGmshWith', 'runGmshWith_', and 'runGmsh_'
-}
runGmsh :: MonadUnliftIO m => GmshT m a -> m (Either GmshError a)
runGmsh = runGmshWith defaultGmshOpts

{- |
@runGmsh_ = 'runGmshWith_' 'defaultGmshOpts'@.

See also: 'runGmshWith', 'runGmshWith_', and 'runGmsh'
-}
runGmsh_ :: MonadUnliftIO m => GmshT m a -> m a
runGmsh_ = runGmshWith_ defaultGmshOpts

embedLow_ :: MonadIO m => IO StatusCode -> GmshT m ()
embedLow_ act = do
  scode <- liftIO act
  checkStatusCode scode

embedLow :: MonadIO m => IO (a, StatusCode) -> GmshT m a
embedLow act = do
  (a, scode) <- liftIO act
  checkStatusCode scode
  pure a

checkStatusCode :: MonadIO m => StatusCode -> GmshT m ()
checkStatusCode scode =
  when (scode /= 0) $ do
    (bsMsg, sCode') <- liftIO Low.gmshLoggerGetLastError
    if sCode' /= 0
      then
        throwIO
          GmshError
            { statusCode = scode
            , errorMessage = T.unpack $ T.decodeUtf8 bsMsg
            }
      else
        throwIO
          GmshError
            { statusCode = scode
            , errorMessage = "<Another exception occurs when retrieving last error>"
            }

data EntityType
  = PointE
  | CurveE
  | CurveLoopE
  | SurfaceE
  | SurfaceLoopE
  | VolumeE
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, Typeable)
  deriving anyclass (Hashable, NFData)

data SEntityType (e :: EntityType) where
  SPointE :: SEntityType 'PointE
  SCurveE :: SEntityType 'CurveE
  SCurveLoopE :: SEntityType 'CurveLoopE
  SSurfaceE :: SEntityType 'SurfaceE
  SSurfaceLoopE :: SEntityType 'SurfaceLoopE
  SVolumeE :: SEntityType 'VolumeE

deriving instance Typeable SEntityType

deriving instance Show (SEntityType e)

deriving instance Eq (SEntityType e)

deriving instance Ord (SEntityType e)

type PointE = 'PointE

type CurveE = 'CurveE

type CurveLoopE = 'CurveLoopE

type SurfaceE = 'SurfaceE

type SurfaceLoopE = 'SurfaceLoopE

type VolumeE = 'VolumeE

newtype Entity (kind :: EntityType) = Entity {rawEntityTag :: Low.EntityTag}
  deriving (Eq, Ord, Generic, Typeable)
  deriving anyclass (Hashable, NFData)

entityTag :: Entity k -> EntityTag k
entityTag = EntityTag . rawEntityTag

unOrient :: Entity entity -> Entity entity
unOrient = coerce $ abs @CInt

instance KnownEntity k => Show (Entity k) where
  showsPrec d e@(Entity tag) =
    showParen (d > 10) $
      showString (init (show (entityVal e)))
        . showChar ' '
        . showsPrec 10 (coerce @_ @CInt tag)

toRawEntity :: HasDimension e => Entity e -> Low.Entity
toRawEntity e@(Entity tag) = Low.Entity (entityDim e) tag

data SomeEntity where
  MkSomeEntity :: HasDimension e => Entity e -> SomeEntity

instance Eq SomeEntity where
  MkSomeEntity l == MkSomeEntity r =
    entityVal l == entityVal r
      && coerce @_ @Low.EntityTag l
      == coerce r
  MkSomeEntity l /= MkSomeEntity r =
    entityVal l /= entityVal r
      || coerce @_ @Low.EntityTag l
      /= coerce r

instance Ord SomeEntity where
  compare = comparing $ \(MkSomeEntity e) ->
    (entityVal e, coerce @_ @Low.EntityTag e)
  {-# INLINE compare #-}

someToRawEntity :: SomeEntity -> Low.Entity
someToRawEntity (MkSomeEntity ent) = toRawEntity ent

toSomeEntity :: Low.Entity -> SomeEntity
toSomeEntity (Low.Entity dim tag) =
  case dim of
    0 -> MkSomeEntity $ Entity @PointE tag
    1 -> MkSomeEntity $ Entity @CurveE tag
    2 -> MkSomeEntity $ Entity @SurfaceE tag
    3 -> MkSomeEntity $ Entity @VolumeE tag
    _ -> error $ "Unexpeceted dimension: " <> show (getDimension dim)

instance TestEquality SEntityType where
  testEquality SPointE SPointE = Just Refl
  testEquality SPointE _ = Nothing
  testEquality SCurveE SCurveE = Just Refl
  testEquality SCurveE _ = Nothing
  testEquality SSurfaceE SSurfaceE = Just Refl
  testEquality SSurfaceE _ = Nothing
  testEquality SCurveLoopE SCurveLoopE = Just Refl
  testEquality SCurveLoopE _ = Nothing
  testEquality SSurfaceLoopE SSurfaceLoopE = Just Refl
  testEquality SSurfaceLoopE _ = Nothing
  testEquality SVolumeE SVolumeE = Just Refl
  testEquality SVolumeE _ = Nothing

fromSomeEntity ::
  forall ent.
  KnownEntity ent =>
  SomeEntity ->
  Maybe (Entity ent)
fromSomeEntity (MkSomeEntity (e :: Entity e)) =
  (sEntityVal' @e `testEquality` sEntityVal' @ent) <&> \case
    Refl -> e

fromRawEntity :: KnownEntity ent => Low.Entity -> Maybe (Entity ent)
fromRawEntity = fromSomeEntity . toSomeEntity

type Point = Entity PointE

type Curve = Entity CurveE

type CurveLoop = Entity CurveLoopE

type Surface = Entity SurfaceE

type SurfaceLoop = Entity SurfaceLoopE

type Volume = Entity VolumeE

class KnownEntity (entityType :: EntityType) where
  entityVal :: proxy entityType -> EntityType
  sEntityVal' :: SEntityType entityType

instance KnownEntity PointE where
  entityVal = const PointE
  sEntityVal' = SPointE

instance KnownEntity CurveE where
  entityVal = const CurveE
  sEntityVal' = SCurveE

instance KnownEntity CurveLoopE where
  entityVal = const CurveLoopE
  sEntityVal' = SCurveLoopE

instance KnownEntity SurfaceE where
  entityVal = const SurfaceE
  sEntityVal' = SSurfaceE

instance KnownEntity SurfaceLoopE where
  entityVal = const SurfaceLoopE
  sEntityVal' = SSurfaceLoopE

instance KnownEntity VolumeE where
  entityVal = const VolumeE
  sEntityVal' = SVolumeE

newtype PhysicalGroup (etype :: EntityType) = PhysicalGroup
  {rawPhysicalTag :: Low.PhysicalTag}
  deriving (Show, Eq, Ord, Typeable, Generic)
  deriving newtype
    (Num, Real, Enum, Bounded, Integral, Storable, NFData, Hashable)

type PhysicalPoint = PhysicalGroup PointE

type PhysicalCurve = PhysicalGroup CurveE

type PhysicalSurface = PhysicalGroup SurfaceE

type PhysicalVolume = PhysicalGroup VolumeE

toRawPhysicalGroup ::
  forall e. HasDimension e => PhysicalGroup e -> Low.PhysicalGroup
toRawPhysicalGroup (PhysicalGroup t) =
  Low.PhysicalGroup (entityDim @e Proxy) t

data SomePhysicalGroup where
  MkSomePhysicalGroup ::
    HasDimension e =>
    PhysicalGroup e ->
    SomePhysicalGroup

toSomePhysicalGroup :: Low.PhysicalGroup -> SomePhysicalGroup
toSomePhysicalGroup (Low.PhysicalGroup dim tag) =
  case dim of
    0 -> MkSomePhysicalGroup $ PhysicalGroup @PointE tag
    1 -> MkSomePhysicalGroup $ PhysicalGroup @CurveE tag
    2 -> MkSomePhysicalGroup $ PhysicalGroup @SurfaceE tag
    3 -> MkSomePhysicalGroup $ PhysicalGroup @VolumeE tag
    _ -> error $ "Unexpeceted dimension: " <> show (getDimension dim)

fromSomePhysicalGroup ::
  forall e. KnownEntity e => SomePhysicalGroup -> Maybe (PhysicalGroup e)
fromSomePhysicalGroup (MkSomePhysicalGroup (pg :: PhysicalGroup ent)) =
  testEquality (sEntityVal' @e) (sEntityVal' @ent) <&> \Refl -> pg

fromRawPhysicalGroup ::
  KnownEntity e =>
  Low.PhysicalGroup ->
  Maybe (PhysicalGroup e)
fromRawPhysicalGroup = fromSomePhysicalGroup . toSomePhysicalGroup

type ParametricCoord (etype :: EntityType) = Vec (EntityDim etype) Double

type family EntityDim (e :: EntityType) :: Nat where
  EntityDim 'PointE = 0
  EntityDim 'CurveE = 1
  EntityDim 'SurfaceE = 2
  EntityDim 'VolumeE = 3
  EntityDim e = TypeError ( 'ShowType e ':<>: 'Text " doesn't have an associated entity dimension!")

type HasDimension entity =
  (KnownNat (EntityDim entity), KnownEntity entity)

entityDim ::
  forall entity proxy.
  HasDimension entity =>
  proxy entity ->
  Dimension
entityDim = const $ fromIntegral $ natVal @(EntityDim entity) Proxy

newtype EntityTag (e :: EntityType) = EntityTag
  {getRawEntityTag :: Low.EntityTag}
  deriving newtype (Num, Show, Eq, Ord, Hashable, Enum)

type PointTag = EntityTag PointE

type CurveTag = EntityTag CurveE

type CurveLoopTag = EntityTag CurveLoopE

type SurfaceTag = EntityTag SurfaceE

type SurfaceLoopTag = EntityTag SurfaceLoopE

type VolumeTag = EntityTag VolumeE

newtype PhysicalTag (e :: EntityType) = PhysicalTag
  {getRawPhysicalTag :: Low.PhysicalTag}
  deriving newtype (Num, Show, Eq, Ord, Hashable, Enum)

type PhysPointTag = PhysicalTag PointE

type PhysCurveTag = PhysicalTag CurveE

type PhysSurfaceTag = PhysicalTag SurfaceE

type PhysVolumeTag = PhysicalTag VolumeE

derivingUnbox
  "Entity"
  [t|forall k. Entity k -> Int|]
  [|fromIntegral . Low.getEntityTag . rawEntityTag|]
  [|Entity . Low.EntityTag . fromIntegral|]

derivingUnbox
  "EntityTag"
  [t|forall k. EntityTag k -> Int|]
  [|fromIntegral . Low.getEntityTag . getRawEntityTag|]
  [|EntityTag . Low.EntityTag . fromIntegral|]

derivingUnbox
  "SomeEntity"
  [t|SomeEntity -> (Int, Int)|]
  [|
    \case
      MkSomeEntity e@(Entity (Low.EntityTag tag) :: Entity e) ->
        (fromIntegral $ getDimension $ entityDim e, fromIntegral tag)
    |]
  [|
    \(dim, tag0) ->
      let tag = Low.EntityTag $ fromIntegral tag0
       in case dim of
            3 -> MkSomeEntity @VolumeE $ Entity tag
            2 -> MkSomeEntity @SurfaceE $ Entity tag
            1 -> MkSomeEntity @CurveE $ Entity tag
            0 -> MkSomeEntity @PointE $ Entity tag
            _ -> error "Cannot happen!"
    |]

data EntitySet = EntitySet
  { pointSet :: Set Point
  , curveSet :: Set Curve
  , surfaceSet :: Set Surface
  , volumeSet :: Set Volume
  }
  deriving (Show, Eq, Ord, Generic, Typeable)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid EntitySet

instance Show SomeEntity where
  showsPrec d (MkSomeEntity s@(Entity e :: Entity e)) =
    showParen (d > 10) $
      let ss = entityVal s
       in showString (init $ show ss) . showChar ' '
            . showsPrec 10 (coerce @_ @CInt e)

-- | Classifies entities, forgetting the orientation.
classifyEntities ::
  Foldable t => t SomeEntity -> EntitySet
classifyEntities = foldMap $ \(MkSomeEntity (e :: Entity e)) ->
  case sEntityVal' @e of
    SPointE ->
      mempty
        { pointSet =
            Set.singleton $
              coerce @(CInt -> CInt) abs e
        }
    SCurveE ->
      mempty
        { curveSet =
            Set.singleton $
              coerce @(CInt -> CInt) abs e
        }
    SCurveLoopE -> mempty
    SSurfaceE ->
      mempty
        { surfaceSet =
            Set.singleton $
              coerce @(CInt -> CInt) abs e
        }
    SSurfaceLoopE -> mempty
    SVolumeE ->
      mempty
        { volumeSet =
            Set.singleton $
              coerce @(CInt -> CInt) abs e
        }

derivingUnbox
  "PhysicalGroup"
  [t|forall k. PhysicalGroup k -> Int|]
  [|fromIntegral . Low.getPhysicalTag . rawPhysicalTag|]
  [|PhysicalGroup . Low.PhysicalTag . fromIntegral|]

derivingUnbox
  "PhysicalTag"
  [t|forall k. PhysicalTag k -> Int|]
  [|fromIntegral . Low.getPhysicalTag . getRawPhysicalTag|]
  [|PhysicalTag . Low.PhysicalTag . fromIntegral|]

data CurveTransfiniteMethod
  = Progression
  | Bump
  | Beta
  | OtherMethod BS.ByteString
  deriving (Read, Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (NFData, Hashable)

renderTransfiniteCurveMethod :: CurveTransfiniteMethod -> BS.ByteString
renderTransfiniteCurveMethod (OtherMethod bs) = bs
renderTransfiniteCurveMethod p = BS.pack $ show p

data PointConfig = Point
  { -- | The coordinate at which the new point will be added.
    pointCoord :: Coord
  , -- | If @> 0@, add a meshing constraint at that point.
    pointMeshSize :: Maybe Double
  , -- | If specified and positive, set the tag explicitly;
    -- otherwise a new tag is selected automatically.
    pointNewTag :: Maybe PointTag
  }
  deriving (Show, Eq, Ord, Generic, Typeable)

data TransSurfaceArrangement
  = Left_
  | Right_
  | AlteranateLeft
  | AlternateRight
  deriving (Read, Show, Eq, Ord, Enum, Bounded, Generic, Typeable)
  deriving anyclass (Hashable, NFData)

data TransSurfaceCorners a
  = TriCorners !a !a !a
  | QuadCorners !a !a !a !a
  deriving (Functor, Traversable, Foldable, Show, Eq, Ord)
  deriving (Generic, Typeable)
  deriving anyclass (Hashable, NFData)

renderTransSurfaceArrangement :: TransSurfaceArrangement -> BS.ByteString
renderTransSurfaceArrangement Left_ = "Left"
renderTransSurfaceArrangement Right_ = "Right"
renderTransSurfaceArrangement a = BS.pack $ show a

{- |
A type that corresponds to some 'Entity' with definite dimension;
i.e. for 'Point', 'Curve', 'Surface' and 'Volume'.

For use with HKD-based operations.
-}
class (HasDimension (EntityTypeOf a), a ~~ Entity (EntityTypeOf a)) => OrdinaryEntity a where
  type EntityTypeOf a :: EntityType

instance HasDimension e => OrdinaryEntity (Entity e) where
  type EntityTypeOf (Entity e) = e

{- | A wrapper function for Boolean operations for entities to
retrieve submesh division by HKD.
-}
withBooleanOp ::
  ( MonadIO m
  , Traversable t
  , Traversable t'
  , ConstraintsB objects
  , TraversableB objects
  , AllB OrdinaryEntity objects
  , ConstraintsB tools
  , TraversableB tools
  , AllB OrdinaryEntity tools
  ) =>
  ( [SomeEntity] ->
    [SomeEntity] ->
    Low.EntityTag ->
    Bool ->
    Bool ->
    GmshT m (V.Vector SomeEntity, V.Vector (V.Vector SomeEntity))
  ) ->
  objects t ->
  tools t' ->
  GmshT
    m
    ( V.Vector SomeEntity
    , objects (Compose t V.Vector)
    , tools (Compose t' V.Vector)
    )
withBooleanOp operation objects tools = do
  (news, subDic) <-
    operation
      ( DL.toList $
          bfoldMapC @OrdinaryEntity (foldMap $ DL.singleton . MkSomeEntity) objects
      )
      ( DL.toList $
          bfoldMapC @OrdinaryEntity (foldMap $ DL.singleton . MkSomeEntity) tools
      )
      (-1)
      True
      True

  let (ob, tl) = flip evalAccum (Sum 0) $ do
        os <-
          btraverseC @OrdinaryEntity
            ( \(ents :: t (Entity e)) -> fmap Compose $
                forM ents $ \_ -> do
                  Sum i <- look
                  add 1
                  pure $ V.mapMaybe fromSomeEntity $ subDic V.! i
            )
            objects
        tls <-
          btraverseC @OrdinaryEntity
            ( \(ents :: t' (Entity e)) -> fmap Compose $
                forM ents $ \_ -> do
                  Sum i <- look
                  add 1
                  pure $ V.mapMaybe fromSomeEntity $ subDic V.! i
            )
            tools
        pure (os, tls)

  pure (news, ob, tl)

{- | An wrapper newtype for converting containers with the
same entity as an entry into HKD-based one.
-}
newtype SingleEntity e h = SingleEntity (h (Entity e))
  deriving (Typeable, Generic)
  deriving anyclass (FunctorB, TraversableB, ConstraintsB)

deriving instance (Show (h (Entity e))) => Show (SingleEntity e h)

-- | Extract entities of specified dimension from HKD-based type.
extractEntity ::
  forall e t h.
  ( HasDimension e
  , Foldable t
  , TraversableB h
  , ConstraintsB h
  , AllB OrdinaryEntity h
  ) =>
  h t ->
  [Entity e]
extractEntity =
  DL.toList . bfoldMapC @OrdinaryEntity \(es :: t (Entity e')) ->
    case testEquality (sEntityVal' @e) (sEntityVal' @e') of
      Just Refl -> foldMap DL.singleton es
      Nothing -> mempty

type family Extruded (e :: EntityType) = e' | e' -> e where
  Extruded 'PointE = 'CurveE
  Extruded 'CurveE = 'SurfaceE
  Extruded 'SurfaceE = 'VolumeE

type family Boundary (e :: EntityType) = e' | e' -> e where
  Boundary 'CurveE = 'PointE
  Boundary 'SurfaceE = 'CurveE
  Boundary 'VolumeE = 'SurfaceE

data ExtrusionMode = WithLateral | NoLateral
  deriving (Show, Eq, Ord, Typeable, Generic)

data Extrusion a where
  Extrusion ::
    Extrusible (Entity e) =>
    { oppositeEntity :: Entity e
    , extrudedEntity :: Entity (Extruded e)
    , lateralEntities :: V.Vector (Entity e)
    } ->
    Extrusion (Entity e)

deriving instance (Extrusible a) => Show (Extrusion a)

instance Foldable Extrusion where
  foldMap f (Extrusion e _ lats) = f e <> foldMap f lats

instance Show1 Extrusion where
  liftShowsPrec _ _ d ext@Extrusion {} = showsPrec d ext

data FocusExtruded a where
  FocusExtruded ::
    Extrusion (Entity e) ->
    FocusExtruded (Entity (Extruded e))

instance Foldable FocusExtruded where
  foldMap f (FocusExtruded (Extrusion _ e _)) = f e

class
  ( OrdinaryEntity a
  , EntityDim (EntityTypeOf a) <= 2
  , KnownEntity (Extruded (EntityTypeOf a))
  ) =>
  Extrusible a

instance
  ( OrdinaryEntity a
  , EntityDim (EntityTypeOf a) <= 2
  , KnownEntity (Extruded (EntityTypeOf a))
  ) =>
  Extrusible a
