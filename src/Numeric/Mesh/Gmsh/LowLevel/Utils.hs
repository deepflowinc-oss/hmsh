{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Numeric.Mesh.Gmsh.LowLevel.Utils where

import Control.Arrow (first)
import Control.Monad ((<$!>), (<=<))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Coerce (Coercible, coerce)
import qualified Data.Vector as V
import Data.Vector.Storable (Storable)
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as MS
import Foreign (FinalizerPtr, addForeignPtrFinalizer, newForeignPtr_)
import Foreign.C (CInt, CSize, CString)
import Foreign.Ptr (Ptr)
import Language.C.Inline (withPtr_, withPtrs, withPtrs_)

withCVector_ ::
  Storable a =>
  (Ptr (Ptr a) -> Ptr CSize -> IO ()) ->
  IO (S.Vector a)
withCVector_ k = uncurry gmshPtrToVector =<< withPtrs_ (uncurry k)

gmshPtrToVector ::
  Storable a => Ptr a -> CSize -> IO (S.Vector a)
gmshPtrToVector cvec len = do
  fptr <- newForeignPtr_ cvec
  addForeignPtrFinalizer gmshFree fptr
  S.force <$!> S.freeze (MS.unsafeFromForeignPtr0 fptr $ fromIntegral len)

withCVector ::
  Storable a =>
  (Ptr (Ptr a) -> Ptr CSize -> IO b) ->
  IO (S.Vector a, b)
withCVector k = do
  ((cvec, len), b) <- withPtrs $ uncurry k
  (,b) <$> gmshPtrToVector cvec len

withCVectorBoxed_ ::
  Storable a =>
  (Ptr (Ptr a) -> Ptr CSize -> IO ()) ->
  IO (V.Vector a)
withCVectorBoxed_ = fmap V.convert . withCVector_

withCVectorBoxed ::
  Storable a =>
  (Ptr (Ptr a) -> Ptr CSize -> IO b) ->
  IO (V.Vector a, b)
withCVectorBoxed = fmap (first V.convert) . withCVector

withCStrings_ ::
  (Ptr (Ptr CString) -> Ptr CSize -> IO ()) -> IO (V.Vector BS.ByteString)
withCStrings_ k = do
  vec <- withCVector_ k
  V.mapM BS.packCString $ V.convert vec

withCString_ :: (Ptr CString -> IO ()) -> IO BS.ByteString
withCString_ = BS8.packCString <=< withPtr_

coerceSVector :: forall b a. (Coercible a b) => S.Vector a -> S.Vector b
coerceSVector = coerce

coerceSVectorM ::
  forall b a f.
  ( forall x y. Coercible x y => Coercible (f x) (f y)
  , Coercible a b
  ) =>
  f (S.Vector a) ->
  f (S.Vector b)
coerceSVectorM = coerce

bool :: Bool -> CInt
{-# INLINE bool #-}
bool = toEnum . fromEnum

withNestedVector ::
  Storable a =>
  (Ptr (Ptr (Ptr a)) -> Ptr (Ptr CSize) -> Ptr CSize -> IO b) ->
  IO (V.Vector (S.Vector a), b)
withNestedVector k = do
  ((xssPtr, lensPtr, lensLen), b) <- withPtrs $ \(xs, lens, len) ->
    k xs lens len
  mvLengths <- gmshPtrToVector lensPtr lensLen
  ptrs <- gmshPtrToVector xssPtr $ fromIntegral $ S.length mvLengths
  xss <- V.zipWithM gmshPtrToVector (V.convert ptrs) $ V.convert mvLengths
  pure (xss, b)

withNestedVector_ ::
  Storable a =>
  (Ptr (Ptr (Ptr a)) -> Ptr (Ptr CSize) -> Ptr CSize -> IO ()) ->
  IO (V.Vector (S.Vector a))
withNestedVector_ = fmap fst . withNestedVector

foreign import ccall "gmshc.h &gmshFree"
  gmshFree :: FinalizerPtr a
