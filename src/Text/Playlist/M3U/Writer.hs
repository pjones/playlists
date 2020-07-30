{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-

This file is part of the Haskell package playlists. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/playlists/LICENSE. No part
of playlists package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Text.Playlist.M3U.Writer (writePlaylist) where

--------------------------------------------------------------------------------
import           Data.ByteString.Lazy.Builder (Builder)
import qualified Data.ByteString.Lazy.Builder as B
import           Data.Text.Encoding           (encodeUtf8)
import           Text.Playlist.Types

--------------------------------------------------------------------------------
writePlaylist :: Playlist -> Builder
writePlaylist Playlist{..} = mconcat
  [ "#EXTM3U\n"
  , foldMap writeTag playlistGlobalTags
  , foldMap (writeTrack . trackRecoverTags) playlistTracks
  ]

--------------------------------------------------------------------------------
writeTrack :: Track -> Builder
writeTrack Track{..} = mconcat
  [ foldMap writeTag trackTags
  , B.byteString (encodeUtf8 trackURL) <> "\n"
  ]

writeTag :: Tag -> Builder
writeTag Tag{..} = B.byteString (encodeUtf8 (getTagName tagName <> ":" <> tagValue))
