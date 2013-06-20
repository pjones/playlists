{-

This file is part of the Haskell package playlists. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/playlists/LICENSE. No part
of playlists package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Text.Playlist.Types
       ( Track (..)
       , Playlist
       , Format (..)
       ) where

--------------------------------------------------------------------------------
import Data.Text (Text)

--------------------------------------------------------------------------------
-- | A single music file or streaming URL.
data Track = Track
  { trackURL   :: Text       -- ^ URL for a file or streaming resource.
  , trackTitle :: Maybe Text -- ^ Optional title.
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- | A list of 'Track's.
type Playlist = [Track]

--------------------------------------------------------------------------------
-- | Playlist formats.
data Format = PLS               -- ^ <http://en.wikipedia.org/wiki/PLS_(file_format)>
            | M3U               -- ^ M3U and M3U8. <http://en.wikipedia.org/wiki/M3U>
              deriving (Read, Show, Eq)
