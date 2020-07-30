{-

This file is part of the Haskell package playlists. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/playlists/LICENSE. No part
of playlists package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

{-# LANGUAGE RecordWildCards #-}

--------------------------------------------------------------------------------
module Text.Playlist.Types
       ( Chunk(..)
       , Track (..)
       , Playlist(..)
       , tracks
       , Format (..)
       ) where

--------------------------------------------------------------------------------
import           Data.Text (Text)

--------------------------------------------------------------------------------
-- | Single playlist chunk. Could be either 'Track' or 'Text' with meta-information.
-- E.g. SCTE35.
data Chunk
  = ChunkTrack Track
  | ChunkExtra Text
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- | A single music file or streaming URL.
data Track = Track
  { trackURL      :: Text        -- ^ URL for a file or streaming resource.
  , trackTitle    :: Maybe Text  -- ^ Optional title.
  , trackDuration :: Maybe Float -- ^ Optional duration in seconds.
  , trackMeta     :: Maybe Text  -- ^ Optional extra tag info.
  , trackTime     :: Maybe Text  -- ^ Optional program datetime.
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- | A list of 'Track's.
data Playlist = Playlist
  { playlistChunks        :: [Chunk]    -- ^ Chunk list.
  , playlistMediaSequence :: Maybe Text -- ^ Optional media sequence number (live).
  }  deriving (Show, Eq)

-- | Obtain 'Track' list from 'Playlist'.
tracks :: Playlist -> [Track]
tracks Playlist{..} = [ track | ChunkTrack track <- playlistChunks ]

--------------------------------------------------------------------------------
-- | Playlist formats.
data Format = PLS               -- ^ <http://en.wikipedia.org/wiki/PLS_(file_format)>
            | M3U               -- ^ M3U and M3U8. <http://en.wikipedia.org/wiki/M3U>
              deriving (Read, Show, Eq)
