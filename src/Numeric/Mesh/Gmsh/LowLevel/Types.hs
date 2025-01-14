{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Numeric.Mesh.Gmsh.LowLevel.Types
  ( StatusCode (..),
    withStatusCode,
    withStatusCode_,
    RGBA (..),
    Dimension (..),
    EntityTag (..),
    MeshFieldTag (..),
    EntityName (..),
    EntityType (..),
    Entity (Entity, entityTag, entityDim),
    parseEntities,
    parseEntitiesM,
    parseEntityTagsM,
    encodeEntities,
    encodeEntitiesM,
    PhysicalTag (..),
    PhysicalName (..),
    PhysicalGroup (PhysicalGroup, physGroupDim, physGroupTag),
    parsePhysicalGroups,
    parsePhysicalGroupsM,
    encodePhysicalGroupsM,
    encodePhysicalGroups,
    Adjacency (..),
    Vec3 (..),
    parseVec3s,
    parseVec3sM,
    encodeVec3s,
    encodeVec3sM,
    Coord (..),
    parseCoords,
    parseCoordsM,
    encodeCoords,
    encodeCoordsM,
    Vec2 (..),
    parseVec2s,
    parseVec2sM,
    encodeVec2s,
    encodeVec2sM,
    NodeTag (..),
    ElementType (..),
    ElementTag (..),
    Vec (V3, V2, V1),
    vecToVector,
    parseVec,
    IsVec (..),
    parseElemsM,
    encodeElemsM,
    mapVec,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (forM_, join)
import qualified Data.ByteString.Char8 as BS8
import Data.Coerce (Coercible, coerce)
import Data.Hashable (Hashable (hashWithSalt))
import Data.Kind (Type)
import Data.String (IsString)
import Data.Tuple (swap)
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as MS
import Data.Vector.Unboxed.Deriving
import Foreign.C.Types
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable
import GHC.Exts (proxy#)
import GHC.Generics (C1, D1, DecidedStrictness (..), FixityI (..), Generic (..), K1 (..), M1 (..), Meta (..), Rec0, S1, SourceStrictness (..), SourceUnpackedness (SourceUnpack), (:*:) (..))
import GHC.TypeNats (KnownNat, Nat, natVal')
import Language.C.Inline (WithPtrs (..), withPtr, withPtr_)
import Type.Reflection (Typeable)

data RGBA = RGBA
  { red :: !CInt
  , green :: !CInt
  , blue :: !CInt
  , alpha :: !CInt
  }
  deriving (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (NFData)

instance WithPtrs RGBA where
  type WithPtrsPtrs RGBA = (Ptr CInt, Ptr CInt, Ptr CInt, Ptr CInt)
  withPtrs k = do
    ((red, green, blue, alpha), x) <- withPtrs $ \rgba -> k rgba
    pure (RGBA {..}, x)

newtype StatusCode = StatusCode {getStatusCode :: CInt}
  deriving (Show, Eq, Ord, Typeable, Generic)
  deriving newtype (Num, Real, Enum, Bounded, Integral, NFData)

withStatusCode_ :: (Ptr CInt -> IO ()) -> IO StatusCode
withStatusCode_ = coerce $ withPtr_ @CInt

withStatusCode :: forall a. (Ptr CInt -> IO a) -> IO (a, StatusCode)
withStatusCode = fmap swap . coerce (withPtr @CInt @a)

newtype Dimension = Dimension {getDimension :: CInt}
  deriving (Show, Eq, Ord, Typeable, Generic)
  deriving newtype (Num, Real, Enum, Bounded, Integral, NFData)

instance Hashable Dimension where
  hashWithSalt salt = hashWithSalt salt . fromEnum
  {-# INLINE hashWithSalt #-}

newtype EntityTag = EntityTag {getEntityTag :: CInt}
  deriving (Show, Eq, Ord, Typeable, Generic)
  deriving newtype (Num, Real, Enum, Bounded, Integral, Storable, NFData)

instance Hashable EntityTag where
  hashWithSalt salt = hashWithSalt salt . fromEnum
  {-# INLINE hashWithSalt #-}

newtype MeshFieldTag = MeshFieldTag {getMeshFieldTag :: CInt}
  deriving (Show, Eq, Ord, Typeable, Generic)
  deriving newtype (Num, Real, Enum, Bounded, Integral, Storable, NFData)
  deriving (Hashable) via EntityTag

newtype EntityName = EntityName {getEntityName :: BS8.ByteString}
  deriving (Show, Eq, Ord, Typeable, Generic)
  deriving newtype (IsString, Hashable, Semigroup, Monoid, NFData)

newtype EntityType = EntityType {getEntityType :: BS8.ByteString}
  deriving (Show, Eq, Ord, Typeable, Generic)
  deriving newtype (IsString, Hashable, Semigroup, Monoid, NFData)

newtype PhysicalName = PhysicalName {getPhysicalName :: BS8.ByteString}
  deriving (Show, Eq, Ord, Typeable, Generic)
  deriving newtype (IsString, Hashable, NFData, Semigroup, Monoid)

newtype DimTag = DimTag_ {dimTag :: S.Vector CInt}
  deriving (Show, Eq, Ord, Typeable)
  deriving newtype (NFData)

{-# COMPLETE DimTag #-}

pattern DimTag :: Dimension -> EntityTag -> DimTag
pattern DimTag {dimVal, tagVal} <-
  DimTag_ [Dimension -> dimVal, EntityTag -> tagVal]
  where
    DimTag (Dimension d) (EntityTag t) = DimTag_ $ S.fromList [d, t]

instance Storable DimTag where
  sizeOf = const $ 2 * sizeOf (0 :: CInt)
  {-# INLINE sizeOf #-}
  alignment = const $ 2 * alignment (0 :: CInt)
  {-# INLINE alignment #-}
  peek ptr = do
    p <- newForeignPtr_ $ castPtr ptr
    fmap DimTag_ . S.freeze =<< MS.clone (MS.unsafeFromForeignPtr0 p 2)
  {-# INLINE peek #-}
  poke ptr (DimTag_ vec) = do
    p <- newForeignPtr_ $ castPtr ptr
    let svec = MS.unsafeFromForeignPtr0 p 2
    S.unsafeCopy svec vec
  {-# INLINE poke #-}

newtype Entity = Entity_ DimTag
  deriving (Eq, Ord, Typeable)
  deriving newtype (NFData, Storable)
  deriving anyclass (Hashable)

instance Generic Entity where
  type
    Rep Entity =
      D1
        ( 'MetaData "Entity" "Numeric.Mesh.Gmsh.LowLevel.Types" "hmsh" 'False)
        ( C1
            ( 'MetaCons "Entity" 'PrefixI 'True)
            ( S1
                ( 'MetaSel ( 'Just "entityDim") 'SourceUnpack 'SourceStrict 'DecidedStrict)
                (Rec0 Dimension)
                :*: S1
                      ( 'MetaSel ( 'Just "entityTag") 'SourceUnpack 'SourceStrict 'DecidedStrict)
                      (Rec0 EntityTag)
            )
        )
  from = \(Entity dim tag) -> coerce $ K1 dim :*: K1 tag
  to = \(coerce -> K1 dim :*: K1 tag) -> Entity dim tag

instance WithPtrs Entity where
  type WithPtrsPtrs Entity = (Ptr CInt, Ptr CInt)
  withPtrs k = do
    ((dim, tag), z) <- withPtrs k
    pure (Entity (Dimension dim) $ EntityTag tag, z)

instance Show Entity where
  showsPrec _ (Entity dim tag) =
    showString "Entity {"
      . showString "entityDim = "
      . showsPrec 10 (getDimension dim)
      . showString ", entityTag = "
      . showsPrec 10 (getEntityTag tag)
      . showChar '}'

{-# COMPLETE Entity #-}

pattern Entity :: Dimension -> EntityTag -> Entity
pattern Entity {entityDim, entityTag} =
  Entity_
    DimTag
      { dimVal = entityDim
      , tagVal = entityTag
      }

parseEntities :: S.Vector CInt -> S.Vector Entity
{-# INLINE parseEntities #-}
parseEntities = S.unsafeCast

parseEntitiesM ::
  ( Functor f
  ) =>
  f (S.Vector CInt) ->
  f (S.Vector Entity)
{-# INLINE parseEntitiesM #-}
parseEntitiesM = fmap S.unsafeCast

parseEntityTagsM ::
  ( Functor f
  ) =>
  f (S.Vector CInt) ->
  f (S.Vector EntityTag)
{-# INLINE parseEntityTagsM #-}
parseEntityTagsM = fmap S.unsafeCast

encodeEntities :: S.Vector Entity -> S.Vector CInt
{-# INLINE encodeEntities #-}
encodeEntities = S.unsafeCast

encodeEntitiesM ::
  ( Functor f
  ) =>
  f (S.Vector Entity) ->
  f (S.Vector CInt)
{-# INLINE encodeEntitiesM #-}
encodeEntitiesM = fmap S.unsafeCast

newtype PhysicalTag = PhysicalTag {getPhysicalTag :: CInt}
  deriving (Show, Eq, Ord, Typeable, Generic)
  deriving newtype (Num, Real, Enum, Bounded, Integral, Storable, NFData)

instance Hashable PhysicalTag where
  hashWithSalt salt = hashWithSalt salt . fromEnum
  {-# INLINE hashWithSalt #-}

newtype PhysicalGroup = PhysicalGroup_ DimTag
  deriving (Eq, Ord, Typeable)
  deriving newtype (NFData, Storable)

instance Generic PhysicalGroup where
  type
    Rep PhysicalGroup =
      D1
        ( 'MetaData "PhysicalGroup" "Numeric.Mesh.Gmsh.LowLevel.Types" "hmsh" 'False)
        ( C1
            ( 'MetaCons "PhysicalGroup" 'PrefixI 'True)
            ( S1
                ( 'MetaSel ( 'Just "physGroupDim") 'SourceUnpack 'SourceStrict 'DecidedStrict)
                (Rec0 Dimension)
                :*: S1
                      ( 'MetaSel ( 'Just "physGroupTag") 'SourceUnpack 'SourceStrict 'DecidedStrict)
                      (Rec0 EntityTag)
            )
        )
  from = \(PhysicalGroup dim tag) -> coerce $ K1 dim :*: K1 tag
  to = \(coerce -> K1 dim :*: K1 tag) -> PhysicalGroup dim tag

instance Show PhysicalGroup where
  showsPrec _ (PhysicalGroup dim tag) =
    showString "PhysicalGroup {"
      . showString "physGroupDim = "
      . showsPrec 10 (getDimension dim)
      . showString ", physGroupTag = "
      . showsPrec 10 (getPhysicalTag tag)
      . showChar '}'

{-# COMPLETE PhysicalGroup #-}

pattern PhysicalGroup :: Dimension -> PhysicalTag -> PhysicalGroup
pattern PhysicalGroup {physGroupDim, physGroupTag} <-
  PhysicalGroup_
    DimTag
      { dimVal = physGroupDim
      , tagVal = coerce -> physGroupTag
      }
  where
    PhysicalGroup d t = PhysicalGroup_ (DimTag d (coerce t))

parsePhysicalGroups :: S.Vector CInt -> S.Vector PhysicalGroup
{-# INLINE parsePhysicalGroups #-}
parsePhysicalGroups = S.unsafeCast

parsePhysicalGroupsM ::
  ( Functor f
  ) =>
  f (S.Vector CInt) ->
  f (S.Vector PhysicalGroup)
{-# INLINE parsePhysicalGroupsM #-}
parsePhysicalGroupsM = fmap S.unsafeCast

encodePhysicalGroups :: S.Vector PhysicalGroup -> S.Vector CInt
{-# INLINE encodePhysicalGroups #-}
encodePhysicalGroups = S.unsafeCast

encodePhysicalGroupsM ::
  ( Functor f
  ) =>
  f (S.Vector PhysicalGroup) ->
  f (S.Vector CInt)
{-# INLINE encodePhysicalGroupsM #-}
encodePhysicalGroupsM = fmap S.unsafeCast

data Adjacency = Adjacency
  { upward :: S.Vector EntityTag
  , downward :: S.Vector EntityTag
  }
  deriving (Show, Eq, Ord, Typeable, Generic)
  deriving anyclass (NFData)

type ParseVectorM a b =
  forall f.
  (Functor f) =>
  f (S.Vector a) ->
  f (S.Vector b)

data Vec3 = Vec3 !CDouble !CDouble !CDouble
  deriving (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (NFData)

instance Num Vec3 where
  fromInteger = join (join Vec3) . fromInteger
  {-# INLINE fromInteger #-}
  Vec3 x y z + Vec3 x' y' z' = Vec3 (x + x') (y + y') (z + z')
  Vec3 x y z - Vec3 x' y' z' = Vec3 (x - x') (y - y') (z - z')
  Vec3 x y z * Vec3 x' y' z' = Vec3 (x * x') (y * y') (z * z')
  negate (Vec3 x y z) = Vec3 (negate x) (negate y) (negate z)
  abs (Vec3 x y z) = Vec3 (abs x) (abs y) (abs z)
  signum (Vec3 x y z) = Vec3 (signum x) (signum y) (signum z)

instance Fractional Vec3 where
  fromRational = join (join Vec3) . fromRational
  {-# INLINE fromRational #-}
  Vec3 x y z / Vec3 x' y' z' = Vec3 (x / x') (y / y') (z / z')
  recip (Vec3 x y z) = Vec3 (recip x) (recip y) (recip z)

instance Num Vec2 where
  fromInteger = join Vec2 . fromInteger
  {-# INLINE fromInteger #-}
  Vec2 x y + Vec2 x' y' = Vec2 (x + x') (y + y')
  Vec2 x y - Vec2 x' y' = Vec2 (x - x') (y - y')
  Vec2 x y * Vec2 x' y' = Vec2 (x * x') (y * y')
  negate (Vec2 x y) = Vec2 (negate x) (negate y)
  abs (Vec2 x y) = Vec2 (abs x) (abs y)
  signum (Vec2 x y) = Vec2 (signum x) (signum y)

instance Fractional Vec2 where
  fromRational = join Vec2 . fromRational
  {-# INLINE fromRational #-}
  Vec2 x y / Vec2 x' y' = Vec2 (x / x') (y / y')
  recip (Vec2 x y) = Vec2 (recip x) (recip y)

type role Vec nominal nominal

mapVec :: forall a b n. (S.Storable a, S.Storable b) => (a -> b) -> Vec n a -> Vec n b
mapVec = coerce $ S.map @a @b

newtype Vec (n :: Nat) a = MkVec {_getVecEntry :: S.Vector a}
  deriving (Eq, Ord, Typeable)

instance (Storable a, Show a) => Show (Vec n a) where
  showsPrec d = showsPrec d . _getVecEntry

vecToVector :: Vec n a -> S.Vector a
vecToVector = coerce

parseVec :: (KnownNat n, Storable a) => S.Vector a -> S.Vector (Vec n a)
parseVec = S.unsafeCast

encodeVec :: (KnownNat n, Storable a) => S.Vector (Vec n a) -> S.Vector a
encodeVec = S.unsafeCast

pattern V1 :: S.Storable a => a -> Vec 1 a
pattern V1 a <-
  MkVec [a]
  where
    V1 a = MkVec $ S.singleton a

{-# COMPLETE V1 #-}

pattern V2 :: S.Storable a => a -> a -> Vec 2 a
pattern V2 a b <-
  MkVec [a, b]
  where
    V2 a b = MkVec [a, b]

{-# COMPLETE V2 #-}

pattern V3 :: S.Storable a => a -> a -> a -> Vec 3 a
pattern V3 a b c <-
  MkVec [a, b, c]
  where
    V3 a b c = MkVec [a, b, c]

{-# COMPLETE V2 #-}

instance (KnownNat n, S.Storable a) => Storable (Vec n a) where
  sizeOf _ = fromIntegral (natVal' @n proxy#) * sizeOf (undefined :: a)
  alignment _ = fromIntegral (natVal' @n proxy#) * alignment (undefined :: a)
  peek ptr = do
    fptr <- newForeignPtr_ (castPtr ptr)
    fmap MkVec . S.freeze
      =<< MS.clone
        ( MS.unsafeFromForeignPtr
            fptr
            0
            (fromIntegral $ natVal' @n proxy#)
        )
  poke ptr (MkVec vec) = do
    forM_ ([0 .. S.length vec - 1] :: [Int]) $ \i ->
      pokeElemOff (castPtr ptr) i (vec S.! i)

parseElemsM ::
  (Functor f, IsVec a) =>
  f (S.Vector (El a)) ->
  f (S.Vector a)
parseElemsM = fmap parseElems

encodeElemsM ::
  (Functor f, IsVec a) =>
  f (S.Vector a) ->
  f (S.Vector (El a))
encodeElemsM = fmap encodeElems

class (KnownNat (Len a), Storable (El a)) => IsVec a where
  type Len a :: Nat
  type El a :: Type
  parseElems :: S.Vector (El a) -> S.Vector a
  default parseElems ::
    Coercible a (Vec (Len a) (El a)) =>
    S.Vector (El a) ->
    S.Vector a
  parseElems = coerce . parseVec @(Len a) @(El a)

  encodeElems :: S.Vector a -> S.Vector (El a)
  default encodeElems ::
    Coercible a (Vec (Len a) (El a)) =>
    S.Vector a ->
    S.Vector (El a)
  encodeElems = encodeVec @(Len a) @(El a) . coerce

instance (KnownNat n, Storable a) => IsVec (Vec n a) where
  type Len (Vec n a) = n
  type El (Vec n a) = a

instance WithPtrs Vec3 where
  type WithPtrsPtrs Vec3 = (Ptr CDouble, Ptr CDouble, Ptr CDouble)
  withPtrs k = do
    ((x, y, z), a) <- withPtrs $ \rgba -> k rgba
    pure (Vec3 x y z, a)

instance Storable Vec3 where
  sizeOf = const $ sizeOf (0 :: CDouble) * 3
  alignment = const $ alignment (0 :: CDouble) * 3
  peek (castPtr @_ @CDouble -> ptr) =
    Vec3 <$> peek ptr <*> peekElemOff ptr 1 <*> peekElemOff ptr 2
  poke (castPtr @_ @CDouble -> ptr) (Vec3 x y z) =
    poke ptr x *> pokeElemOff ptr 1 y *> pokeElemOff ptr 2 z

parseVec3s :: S.Vector CDouble -> S.Vector Vec3
parseVec3s = S.unsafeCast

parseVec3sM :: ParseVectorM CDouble Vec3
parseVec3sM = fmap S.unsafeCast

encodeVec3s :: S.Vector Vec3 -> S.Vector CDouble
encodeVec3s = S.unsafeCast

encodeVec3sM :: ParseVectorM Vec3 CDouble
encodeVec3sM = fmap S.unsafeCast

newtype Coord = Coord {getCoord :: Vec3}
  deriving (Show, Eq, Ord, Generic, Typeable)
  deriving newtype (NFData, Storable, WithPtrs)

parseCoords :: S.Vector CDouble -> S.Vector Coord
parseCoords = S.unsafeCast

parseCoordsM :: ParseVectorM CDouble Coord
parseCoordsM = fmap S.unsafeCast

encodeCoords :: S.Vector Coord -> S.Vector CDouble
encodeCoords = S.unsafeCast

encodeCoordsM :: ParseVectorM Coord CDouble
encodeCoordsM = fmap S.unsafeCast

data Vec2 = Vec2 !CDouble !CDouble
  deriving (Show, Eq, Ord, Generic, Typeable)
  deriving anyclass (NFData)

instance WithPtrs Vec2 where
  type WithPtrsPtrs Vec2 = (Ptr CDouble, Ptr CDouble)
  withPtrs k = do
    ((x, y), a) <- withPtrs $ \rgba -> k rgba
    pure (Vec2 x y, a)

instance Storable Vec2 where
  sizeOf = const $ sizeOf (0 :: CDouble) * 3
  alignment = const $ alignment (0 :: CDouble) * 3
  peek (castPtr @_ @CDouble -> ptr) =
    Vec2 <$> peek ptr <*> peekElemOff ptr 1
  poke (castPtr @_ @CDouble -> ptr) (Vec2 x y) =
    poke ptr x *> pokeElemOff ptr 1 y

parseVec2s :: S.Vector CDouble -> S.Vector Vec2
parseVec2s = S.unsafeCast

parseVec2sM :: ParseVectorM CDouble Vec2
parseVec2sM = fmap S.unsafeCast

encodeVec2s :: S.Vector Vec2 -> S.Vector CDouble
encodeVec2s = S.unsafeCast

encodeVec2sM :: ParseVectorM Vec2 CDouble
encodeVec2sM = fmap S.unsafeCast

newtype NodeTag = NodeTag {getNodeTag :: CSize}
  deriving (Show, Eq, Ord, Typeable, Generic)
  deriving newtype (Num, Real, Enum, Bounded, Integral, Storable, NFData)

newtype ElementType = ElementType {getElementType :: CInt}
  deriving (Show, Eq, Ord, Typeable, Generic)
  deriving newtype (Num, Real, Enum, Bounded, Integral, Storable, NFData)

newtype ElementTag = ElementTag {getElementTag :: CSize}
  deriving (Show, Eq, Ord, Typeable, Generic)
  deriving newtype (Num, Real, Enum, Bounded, Integral, Storable, NFData)

derivingUnbox
  "Entity"
  [t|Entity -> (Int, Int)|]
  [|\(Entity dim tag) -> (fromIntegral dim, fromIntegral tag)|]
  [|\(dim, tag) -> Entity (fromIntegral dim) (fromIntegral tag)|]

derivingUnbox
  "PhysicalGroup"
  [t|PhysicalGroup -> (Int, Int)|]
  [|\(PhysicalGroup dim tag) -> (fromIntegral dim, fromIntegral tag)|]
  [|\(dim, tag) -> PhysicalGroup (fromIntegral dim) (fromIntegral tag)|]
