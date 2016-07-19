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
import Data.Text (Text)
import Data.ByteString (ByteString)

--------------------------------------------------------------------------------
import Text.Playlist.Types

--------------------------------------------------------------------------------
resolve :: (Monad m)
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
resolve = undefined
