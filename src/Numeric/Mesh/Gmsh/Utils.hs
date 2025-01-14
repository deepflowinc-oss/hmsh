{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Numeric.Mesh.Gmsh.Utils where

import Barbies
import qualified Data.ByteString.Char8 as BS
import Data.Coerce (Coercible, coerce)
import Data.Functor.Contravariant (Contravariant, phantom)
import Data.Monoid (Endo (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import Lens.Micro.Contra (Fold)

toBS :: String -> BS.ByteString
toBS = T.encodeUtf8 . T.pack

chunksOf :: G.Vector v a => Int -> v a -> V.Vector (v a)
chunksOf n = V.unfoldr go
  where
    go xs
      | G.null xs = Nothing
      | otherwise = Just (G.splitAt n xs)

class Vacuous a

instance Vacuous a

data ExistsC c f where
  SomeC :: forall c f a. c a => f a -> ExistsC c f

type Exists = ExistsC Vacuous

noEffect :: (Applicative f, Contravariant f) => f a
noEffect = phantom (pure ())
{-# INLINE noEffect #-}

foldring :: (forall b. (a -> b -> b) -> b -> s -> b) -> Fold s a
foldring fr f = phantom . fr (\a fa -> f a *> fa) noEffect

bfolded :: TraversableB h => Fold (h t) (Exists t)
bfolded = foldring $ \f z t -> appEndo (bfoldMap (Endo #. (f . SomeC)) t) z

bfoldedC ::
  forall c h t.
  (TraversableB h, ConstraintsB h, AllB c h) =>
  Fold (h t) (ExistsC c t)
bfoldedC = foldring $ \f z t ->
  appEndo (bfoldMapC @c (Endo #. (f . SomeC)) t) z

(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
{-# INLINE (#.) #-}
