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
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

--------------------------------------------------------------------------------
module Text.Playlist.Types where

--------------------------------------------------------------------------------
import qualified Data.Foldable            as F
import           Data.List                (find)
import           Data.Maybe               (catMaybes)
import           Data.String              (IsString)
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Data.Time
import           Data.Time.Format.ISO8601
import           Text.Read                (readMaybe)

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
  { trackURL      :: Text           -- ^ URL for a file or streaming resource.
  , trackTitle    :: Maybe Text     -- ^ Optional title.
  , trackDuration :: Maybe Float    -- ^ Optional duration in seconds.
  , trackDateTime :: Maybe ZonedTime
  , trackTags     :: [Tag]
  } deriving (Show)

trackParseTags :: Track -> Track
trackParseTags track@Track{..} = track
  { trackTitle    = parsedTitle
  , trackDuration = parsedDuration
  , trackDateTime = parseEXT_X_PROGRAM_DATETIME trackTags
  }
  where
    (parsedDuration, parsedTitle) =
      case parseEXTINF trackTags of
        Nothing     -> (Nothing, Nothing)
        Just (d, t) -> (Just d, if Text.null t then Nothing else Just t)

parseEXT_X_PROGRAM_DATETIME :: [Tag] -> Maybe ZonedTime
parseEXT_X_PROGRAM_DATETIME tags = do
  Tag{..} <- lookupTag "#EXT-X-PROGRAM-DATETIME" tags
  iso8601ParseM (Text.unpack tagValue)

parseEXTINF :: [Tag] -> Maybe (Float, Text)
parseEXTINF tags = do
  Tag{..} <- lookupTag "#EXTINF" tags
  let (before, after) = Text.break (== ',') tagValue
      mDuration = readMaybe (Text.unpack before)
      title = Text.drop 1 after
  duration <- mDuration
  return (duration, title)

recoverEXTINF :: Maybe Float -> Maybe Text -> Maybe Tag
recoverEXTINF mDuration mTitle = do
  duration <- mDuration
  return Tag
    { tagName = "#EXTINF"
    , tagValue = Text.pack (show duration) <> "," <> F.fold mTitle
    }

recoverEXT_X_PROGRAM_DATETIME :: ZonedTime -> Tag
recoverEXT_X_PROGRAM_DATETIME zt = Tag
  { tagName = "#EXT-X-PROGRAM-DATETIME"
  , tagValue = Text.pack (iso8601Show zt)
  }

overrideTags :: [Tag] -> [Tag] -> [Tag]
overrideTags old new = filter notInNew old <> new
  where
    notInNew tag = tagName tag `notElem` map tagName new

trackRecoverTags :: Track -> Track
trackRecoverTags track@Track{..} = track
  { trackTags = overrideTags trackTags recoveredTags }
  where
    recoveredTags = catMaybes
      [ recoverEXTINF trackDuration trackTitle
      , recoverEXT_X_PROGRAM_DATETIME <$> trackDateTime
      ]

--------------------------------------------------------------------------------
-- | A list of 'Track's.
data Playlist = Playlist
  { playlistGlobalTags :: [Tag]     -- ^ Global playlist tags.
  , playlistTracks     :: [Track]   -- ^ A list of tracks.
  } deriving (Show)

--------------------------------------------------------------------------------
-- | Playlist formats.
data Format = PLS               -- ^ <http://en.wikipedia.org/wiki/PLS_(file_format)>
            | M3U               -- ^ M3U and M3U8. <http://en.wikipedia.org/wiki/M3U>
              deriving (Read, Show, Eq)
