{-

This file is part of the Haskell package playlists. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/playlists/LICENSE. No part
of playlists package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

-- | Wrapper around doctest to make sure examples in the source code work.
module Main (main) where

import System.Environment
import Test.DocTest

-- Totally stupid right now.  Will search the directory later.
--
-- @
--   $ find src -type f -name '*.hs'
-- @
files :: [String]
files =
  [ "util/playlist.hs",
    "src/Text/Playlist.hs",
    "src/Text/Playlist/M3U/Writer.hs",
    "src/Text/Playlist/M3U/Reader.hs",
    "src/Text/Playlist/PLS/Writer.hs",
    "src/Text/Playlist/PLS/Reader.hs",
    "src/Text/Playlist/Internal/ReadWrite.hs",
    "src/Text/Playlist/Internal/Attoparsec.hs",
    "src/Text/Playlist/Internal/Format.hs",
    "src/Text/Playlist/Internal/Resolve.hs",
    "src/Text/Playlist/Types.hs"
  ]

-- | GHC flags needed by doctest.  Would be better to fetch these from
-- cabal directly though.
flags :: [String]
flags = ["-isrc"]

-- | Check the docs.
main :: IO ()
main = do
  args <- getArgs
  doctest (args <> flags <> files)
