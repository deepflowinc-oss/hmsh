{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Numeric.Mesh.Gmsh.Option
  ( setOption,
    setBoolOption,
    setNumberOption,
    setColorOption,
    setStringOption,
    OptionArg (..),
    withOption,
  )
where

import qualified Data.ByteString.Char8 as BS
import Data.Proxy (Proxy (Proxy))
import Data.String (IsString (..))
import Foreign.C (CInt)
import Foreign.C.Types (CDouble)
import GHC.Generics (Generic)
import GHC.OverloadedLabels
import GHC.TypeLits (KnownSymbol, symbolVal)
import Numeric (readHex)
import qualified Numeric.Mesh.Gmsh.LowLevel as Low
import Numeric.Mesh.Gmsh.Types
import Numeric.Mesh.Gmsh.Utils
import UnliftIO

data OptionArg
  = StringArg !String
  | NumberArg !CDouble
  | BoolArg !Bool
  | ColorArg Low.RGBA
  deriving (Show, Eq, Ord, Generic, Typeable)

-- | For string arguments only
instance IsString OptionArg where
  fromString = StringArg

-- | For number arguments only
instance Num OptionArg where
  fromInteger = NumberArg . fromInteger
  NumberArg a + NumberArg b = NumberArg $ a + b
  _ + _ = error "(+): Not a number arg!"
  NumberArg a - NumberArg b = NumberArg $ a - b
  _ - _ = error "(-): Not a number arg!"
  NumberArg a * NumberArg b = NumberArg $ a * b
  _ * _ = error "(*): Not a number arg!"
  abs (NumberArg a) = NumberArg $ abs a
  abs _ = error "abs: Not a number arg!"
  signum (NumberArg a) = NumberArg $ signum a
  signum _ = error "signum: Not a number arg!"

-- | For number arguments only
instance Fractional OptionArg where
  fromRational = NumberArg . fromRational
  recip (NumberArg p) = NumberArg $ recip p
  recip _ = error "recip: Not a number arg"

withNumberArg ::
  String -> (CDouble -> CDouble) -> OptionArg -> OptionArg
withNumberArg _ f (NumberArg x) = NumberArg $ f x
withNumberArg f _ x = error $ f <> ": not a number arg! (" <> show x <> ")"

instance Floating OptionArg where
  pi = NumberArg pi
  exp = withNumberArg "exp" exp
  log = withNumberArg "log" log
  sin = withNumberArg "sin" sin
  cos = withNumberArg "cos" cos
  tan = withNumberArg "tan" tan
  asin = withNumberArg "asin" asin
  acos = withNumberArg "acos" acos
  atan = withNumberArg "atan" atan
  sinh = withNumberArg "sinh" sinh
  cosh = withNumberArg "cosh" cosh
  tanh = withNumberArg "tanh" tanh
  asinh = withNumberArg "asinh" asinh
  acosh = withNumberArg "acosh" acosh
  atanh = withNumberArg "atanh" atanh

{- | Experimental; for color options: "#fff" or  "#ffffff"  for rgb with a = 255;
"#ffff" or "#ffffffff" for rgba.

N.B. We are currently doesn't do any static check for the above.
-}
instance KnownSymbol s => IsLabel s OptionArg where
  fromLabel =
    let symb = symbolVal @s Proxy
     in ColorArg $ readRGBA symb

readRGBA :: [Char] -> Low.RGBA
readRGBA = \case
  [r, g, b] ->
    Low.RGBA
      { red = readHex' [r, r]
      , green = readHex' [g, g]
      , blue = readHex' [b, b]
      , alpha = 255
      }
  [r, g, b, a] ->
    Low.RGBA
      { red = readHex' [r, r]
      , green = readHex' [g, g]
      , blue = readHex' [b, b]
      , alpha = readHex' [a, a]
      }
  [r1, r2, g1, g2, b1, b2] ->
    Low.RGBA
      { red = readHex' [r1, r2]
      , green = readHex' [g1, g2]
      , blue = readHex' [b1, b2]
      , alpha = 255
      }
  [r1, r2, g1, g2, b1, b2, a1, a2] ->
    Low.RGBA
      { red = readHex' [r1, r2]
      , green = readHex' [g1, g2]
      , blue = readHex' [b1, b2]
      , alpha = readHex' [a1, a2]
      }
  _ -> error "malformed color specification!"

readHex' :: String -> CInt
readHex' hex = case readHex hex of
  [(i, "")] -> i
  _ -> error $ "Not a complete hex: " <> hex

setOption :: MonadIO m => String -> OptionArg -> GmshT m ()
setOption opts (StringArg str) =
  embedLow_ $ Low.gmshOptionSetString (toBS opts) (toBS str)
setOption opts (NumberArg num) =
  embedLow_ $ Low.gmshOptionSetNumber (toBS opts) num
setOption opts (ColorArg rgba) =
  embedLow_ $ Low.gmshOptionSetColor (toBS opts) rgba
setOption opts (BoolArg b) =
  embedLow_ $ Low.gmshOptionSetNumber (toBS opts) (if b then 1 else 0)

setStringOption :: MonadIO m => String -> String -> GmshT m ()
setStringOption opt = setOption opt . StringArg

setNumberOption :: MonadIO m => String -> CDouble -> GmshT m ()
setNumberOption opt = setOption opt . NumberArg

setColorOption :: MonadIO m => String -> Low.RGBA -> GmshT m ()
setColorOption opt = setOption opt . ColorArg

setBoolOption :: MonadIO m => String -> Bool -> GmshT m ()
setBoolOption opt = setOption opt . BoolArg

-- | Executing monadic action, temporarilly changing the option value, and finally recover to the original option value.
withOption ::
  MonadUnliftIO m =>
  String ->
  OptionArg ->
  GmshT m a ->
  GmshT m a
withOption opt arg act =
  bracket
    (getCorrespondingOpt opt arg <* setOption opt arg)
    (setOption opt)
    (const act)

getCorrespondingOpt :: MonadIO m => String -> OptionArg -> GmshT m OptionArg
getCorrespondingOpt opt StringArg {} =
  fmap (StringArg . BS.unpack) $
    embedLow $ Low.gmshOptionGetString (BS.pack opt)
getCorrespondingOpt opt NumberArg {} =
  fmap NumberArg $
    embedLow $ Low.gmshOptionGetNumber (BS.pack opt)
getCorrespondingOpt opt BoolArg {} =
  fmap (BoolArg . (/= 0)) $
    embedLow $ Low.gmshOptionGetNumber (BS.pack opt)
getCorrespondingOpt opt ColorArg {} =
  fmap ColorArg $
    embedLow $ Low.gmshOptionGetColor (BS.pack opt)
