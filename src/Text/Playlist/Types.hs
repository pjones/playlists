{-

This file is part of the Haskell package playlists. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/playlists/LICENSE. No part
of playlists package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

--------------------------------------------------------------------------------
module Text.Playlist.Types where

--------------------------------------------------------------------------------
import           Data.List   (find)
import           Data.String (IsString)
import           Data.Text   (Text)

newtype TagName = TagName { getTagName :: Text }
  deriving newtype (Eq, Ord, Show, IsString)

data Tag = Tag
  { tagName  :: TagName
  , tagValue :: Text
  } deriving (Eq, Show)

lookupTag :: TagName -> [Tag] -> Maybe Tag
lookupTag name = find ((== name) . tagName)

--------------------------------------------------------------------------------
-- | A single music file or streaming URL.
data Track = Track
  { trackURL      :: Text        -- ^ URL for a file or streaming resource.
  , trackTitle    :: Maybe Text  -- ^ Optional title.
  , trackDuration :: Maybe Float -- ^ Optional duration in seconds.
  , trackTags     :: [Tag]
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- | A list of 'Track's.
data Playlist = Playlist
  { playlistGlobalTags :: [Tag]     -- ^ Global playlist tags.
  , playlistTracks     :: [Track]   -- ^ A list of tracks.
  } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Playlist formats.
data Format = PLS               -- ^ <http://en.wikipedia.org/wiki/PLS_(file_format)>
            | M3U               -- ^ M3U and M3U8. <http://en.wikipedia.org/wiki/M3U>
              deriving (Read, Show, Eq)
