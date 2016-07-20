{-

This file is part of the Haskell package playlists. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/playlists/LICENSE. No part
of playlists package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Main where

--------------------------------------------------------------------------------
import Test.Hspec

--------------------------------------------------------------------------------
import qualified M3USpec
import qualified PLSSpec
import qualified ResolveSpec

--------------------------------------------------------------------------------
main :: IO ()
main = hspec $ do
  describe "M3U"     M3USpec.spec
  describe "PLS"     PLSSpec.spec
  describe "Resolve" ResolveSpec.spec
