{-

This file is part of the Haskell package playlists. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/playlists/LICENSE. No part
of playlists package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Text.Playlist
       ( -- * Playlist Types
         Track (..)
       , Playlist

         -- * Playlist Formats
       , Format (..)

         -- * Parsing and Generating
       , parsePlaylist
       , generatePlaylist

         -- * Utility Functions
       , fileNameToFormat
       , appendExtension
       , resolve
       ) where

--------------------------------------------------------------------------------
import Text.Playlist.Internal.Format
import Text.Playlist.Internal.ReadWrite
import Text.Playlist.Types
import Text.Playlist.Internal.Resolve
