{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeApplications #-}

module Numeric.Mesh.Gmsh.Helper.Parser where

import qualified Data.Vector as V
import Numeric.Mesh.Gmsh.Types
import Numeric.Mesh.Gmsh.Utils
import Text.Regex.Applicative

entityP :: KnownEntity e => RE SomeEntity (Entity e)
entityP = msym fromSomeEntity

pointP :: RE SomeEntity Point
pointP = entityP @PointE

curveP :: RE SomeEntity Curve
curveP = entityP @CurveE

surfaceP :: RE SomeEntity Surface
surfaceP = entityP @SurfaceE

volumeP :: RE SomeEntity Volume
volumeP = entityP @VolumeE

extrudedP :: KnownEntity (Extruded e) => RE SomeEntity (Entity (Extruded e))
extrudedP = entityP

extrusionP :: Extrusible e => ExtrusionMode -> RE SomeEntity (Extrusion e)
extrusionP eMode =
  Extrusion <$> entityP <*> entityP
    <*> ( V.fromList
            <$> case eMode of
              NoLateral -> pure mempty
              WithLateral -> few entityP
        )

someExtrusionP :: ExtrusionMode -> EntityType -> RE SomeEntity (ExistsC Extrusible Extrusion)
someExtrusionP eMode CurveE = SomeC <$> extrusionP @Curve eMode
someExtrusionP eMode PointE = SomeC <$> extrusionP @Point eMode
someExtrusionP eMode SurfaceE = SomeC <$> extrusionP @Surface eMode
someExtrusionP _ e = error $ "Unsupported extrusion: " <> show e
