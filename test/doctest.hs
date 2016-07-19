{-

This file is part of the Haskell package playlists. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/playlists/LICENSE. No part
of playlists package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
-- | Wrapper around doctest to make sure examples in the source code work.
module Main (main) where

--------------------------------------------------------------------------------
import Control.Applicative
import System.Environment
import Test.DocTest

--------------------------------------------------------------------------------
-- Totally stupid right now.  Will search the directory later.
files :: IO [String]
files = fmap ("util/playlist.hs" :) files' where
  files' = return ["src/Text/Playlist.hs"]

--------------------------------------------------------------------------------
-- | GHC flags needed by doctest.  Would be better to fetch these from
-- cabal directly though.
flags :: [String]
flags = ["-isrc"]

--------------------------------------------------------------------------------
-- | Check the docs.
main :: IO ()
main = doctest =<< (++) <$> getArgs <*> fmap (flags ++) files
