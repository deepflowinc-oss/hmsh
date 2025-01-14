{-# LANGUAGE NoMonomorphismRestriction #-}

module Numeric.Mesh.Gmsh.Fltk
  ( initialize,
    run,
    update,
    wait,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Numeric.Mesh.Gmsh (embedLow_)
import Numeric.Mesh.Gmsh.LowLevel (gmshFltkInitialize, gmshFltkRun, gmshFltkUpdate, gmshFltkWait)
import Numeric.Mesh.Gmsh.Types (GmshT)

initialize :: MonadIO m => GmshT m ()
initialize = embedLow_ gmshFltkInitialize

run :: MonadIO m => GmshT m ()
run = embedLow_ gmshFltkRun

update :: MonadIO m => GmshT m ()
update = embedLow_ gmshFltkUpdate

wait :: MonadIO m => Double -> GmshT m ()
wait = embedLow_ . gmshFltkWait . realToFrac
