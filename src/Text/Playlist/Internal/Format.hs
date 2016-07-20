{-

This file is part of the Haskell package playlists. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/playlists/LICENSE. No part
of playlists package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Text.Playlist.Internal.Format
       ( fileNameToFormat
       , appendExtension
       ) where

--------------------------------------------------------------------------------
import Data.Char (toLower)
import System.FilePath (takeExtension)

--------------------------------------------------------------------------------
import Text.Playlist.Types

--------------------------------------------------------------------------------
-- | Try to figure out a file's format from it's file extension.
--
-- >>> fileNameToFormat "foo.m3u"
-- Just M3U
--
-- >>> fileNameToFormat "foo.txt"
-- Nothing
fileNameToFormat :: FilePath -> Maybe Format
fileNameToFormat ext = case map toLower $ takeExtension ext of
  ".m3u"  -> Just M3U
  ".m3u8" -> Just M3U
  ".pls"  -> Just PLS
  _       -> Nothing

--------------------------------------------------------------------------------
-- | Given a file name that does not have a file extension, return a
-- file name with the appropriate extension included based on the
-- given format.
--
-- >>> appendExtension M3U "foo"
-- "foo.m3u"
appendExtension :: Format -> FilePath -> FilePath
appendExtension M3U = (++ ".m3u")
appendExtension PLS = (++ ".pls")
