{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Numeric.Mesh.Gmsh
  ( module Numeric.Mesh.Gmsh.Types,
    module Numeric.Mesh.Gmsh.Option,
    module Numeric.Mesh.Gmsh.Model.Mesh,
    module Numeric.Mesh.Gmsh.Model,
    open,
    merge,
    write,
    clear,
  )
where

import Control.Monad.IO.Class (MonadIO)
import qualified Numeric.Mesh.Gmsh.LowLevel as Low
import Numeric.Mesh.Gmsh.Model
import Numeric.Mesh.Gmsh.Model.Mesh hiding (TransfiniteMeshing (..), setTransfiniteCurve, setTransfiniteSurface, setTransfiniteVolume)
import Numeric.Mesh.Gmsh.Option
import Numeric.Mesh.Gmsh.Types
import Numeric.Mesh.Gmsh.Utils

{- |
Open a file. Equivalent to the File -> Open menu in the Gmsh app. Handling
of the file depends on its extension and/or its contents: opening a file
with model data will create a new model.
-}
open :: MonadIO m => FilePath -> GmshT m ()
open = embedLow_ . Low.gmshOpen . toBS

{- |
Merge a file. Equivalent to the File->Merge menu in the Gmsh app.
Handling of the file depends on its extension and/or its contents. Merging
a file with model data will add the data to the current model.
-}
merge :: MonadIO m => String -> GmshT m ()
merge = embedLow_ . Low.gmshMerge . toBS

-- | Write a file. The export format is determined by the file extension.
write :: MonadIO m => String -> GmshT m ()
write = embedLow_ . Low.gmshWrite . toBS

-- | Clear all loaded models and post-processing data, and add a new empty model.
clear :: MonadIO m => GmshT m ()
clear = embedLow_ Low.gmshClear
