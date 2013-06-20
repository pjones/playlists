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
       ( Track (..)
       , Playlist
       , Format (..)
       , parsePlaylist
       , generatePlaylist
       ) where

--------------------------------------------------------------------------------
import qualified Data.Attoparsec.ByteString as Atto
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Builder as BL
import qualified Text.Playlist.M3U.Reader as M3U
import qualified Text.Playlist.M3U.Writer as M3U
import qualified Text.Playlist.PLS.Reader as PLS
import qualified Text.Playlist.PLS.Writer as PLS
import Text.Playlist.Types

--------------------------------------------------------------------------------
parsePlaylist :: Format -> ByteString -> Either String Playlist
parsePlaylist M3U = Atto.parseOnly M3U.parsePlaylist
parsePlaylist PLS = Atto.parseOnly PLS.parsePlaylist

--------------------------------------------------------------------------------
generatePlaylist :: Format -> Playlist -> BL.ByteString
generatePlaylist M3U = BL.toLazyByteString . M3U.writePlaylist
generatePlaylist PLS = BL.toLazyByteString . PLS.writePlaylist
