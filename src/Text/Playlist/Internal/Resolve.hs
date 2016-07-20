{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}


{-

This file is part of the Haskell package playlists. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/playlists/LICENSE. No part
of playlists package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Text.Playlist.Internal.Resolve
       ( resolve
       ) where

--------------------------------------------------------------------------------
import Control.Monad
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
import Text.Playlist.Internal.Format
import Text.Playlist.Internal.ReadWrite
import Text.Playlist.Types

--------------------------------------------------------------------------------
-- Internal type to track when a playlist may need to be processed a
-- another time.  (Such as when a remote playlist refers to other
-- remote playlists.)
data Resolution = Flat Playlist | Again Playlist

--------------------------------------------------------------------------------
-- | If the given 'Playlist' contains tracks that reference remote
-- playlists, this function will recursively download and process
-- these playlists.  Returns a flattened playlist that should not
-- contain any references to other playlists.
--
-- You supply the downloading function as the second argument.  Use
-- whichever HTTP library that makes you happy.
--
-- There are two error conditions that are ignored by this function:
--
--   1. The nesting of playlists exceeds a (hard-coded) limit.  In
--      this case no playlists beyond the limit are processed.  Open a
--      pull request if you'd like to have a resolveN function that
--      allows you to specific the depth limit or one that returns an
--      error.
--
--   2. A downloaded playlist contains a syntax error.  In this case
--      the playlist is consider to have no tracks and is ignored.
--      Open a pull request if you want a version of this function
--      that returns some sort of an error instead of ignoring bad
--      playlists.
resolve :: forall m. (Monad m)
        => Playlist
        -- ^ A 'Playlist' that may contain references to other
        -- playlists.

        -> (Text -> m ByteString)
        -- ^ Downloading function.  This function should take a URL
        -- and return the body of the document found at that URL.
        --
        -- It's expected that the URL points to another playlist that
        -- needs to be parsed and possibly resolved.

        -> m Playlist
        -- ^ A fully resolved 'Playlist'.  (All tracks should be files
        -- and not links to other playlists.)
resolve playlist download = go 10 playlist where

  ----------------------------------------------------------------------------
  -- Recursively process tracks in the 'Playlist' with a maximum depth
  -- of @n@.
  go :: (Monad m) => Int -> Playlist -> m Playlist
  go _ [] = return []
  go 0 xs = return xs
  go n xs = fmap join $ forM xs $ \track -> do
    r <- process track

    case r of
      Flat p  -> return p
      Again p -> go (n - 1) p

  ----------------------------------------------------------------------------
  -- Process a single track.
  process :: Track -> m Resolution
  process t@Track {..} =
    case fileNameToFormat (Text.unpack trackURL) of
      Nothing -> return (Flat [t])
      Just f  -> Again <$> parse f trackURL

  ----------------------------------------------------------------------------
  -- Helper function to download and parse a remote playlist.
  parse :: (Monad m) => Format -> Text -> m Playlist
  parse format url = do
    raw <- download url

    case parsePlaylist format raw of
      Left _      -> return []
      Right plist -> return plist
