{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_SchemeInterpreter (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "SchemeInterpreter"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Very basic interpreter for the Scheme programming language."
copyright :: String
copyright = ""
homepage :: String
homepage = ""
