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
import           Data.Text                    (Text)
import           Data.Text.Encoding           (encodeUtf8)
import           Text.Playlist.Types

--------------------------------------------------------------------------------
writePlaylist :: Playlist -> Builder
writePlaylist x = B.byteString "#EXTM3U\n" <> mconcat (map writeChunk cs)
  where
    cs = playlistChunks x

--------------------------------------------------------------------------------
writeChunk :: Chunk -> Builder
writeChunk (ChunkExtra x) = B.byteString (encodeUtf8 x)
writeChunk (ChunkTrack x) = writeTrack x

--------------------------------------------------------------------------------
writeTrack :: Track -> Builder
writeTrack x =
  writeTitleAndLength x <>
  B.byteString (encodeUtf8 $ trackURL x) <>
  B.charUtf8 '\n'

--------------------------------------------------------------------------------
writeTitleAndLength :: Track -> Builder
writeTitleAndLength (Track _ Nothing Nothing Nothing Nothing) = mempty
writeTitleAndLength Track{..} =
  B.byteString "#EXTINF:"   <>
  writeLength trackDuration <>
  B.byteString ","          <>
  writeTitle trackTitle     <>
  B.charUtf8 '\n'

--------------------------------------------------------------------------------
writeLength :: Maybe Float -> Builder
writeLength Nothing  = mempty
writeLength (Just l) = B.stringUtf8 (show l)

--------------------------------------------------------------------------------
writeTitle :: Maybe Text -> Builder
writeTitle Nothing  = mempty
writeTitle (Just x) = B.byteString (encodeUtf8 x)

