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
       , fileNameToFormat
       , appendExtension
       ) where

--------------------------------------------------------------------------------
import qualified Data.Attoparsec.ByteString as Atto
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Builder as BL
import System.FilePath (takeExtension)
import qualified Text.Playlist.M3U.Reader as M3U
import qualified Text.Playlist.M3U.Writer as M3U
import qualified Text.Playlist.PLS.Reader as PLS
import qualified Text.Playlist.PLS.Writer as PLS
import Text.Playlist.Types

--------------------------------------------------------------------------------
-- | Parse a playlist from a @ByteString@.  Parsing may fail in which
-- case an error message is returned in @Left@.
--
-- > content <- BS.getContents
-- > case parsePlaylist M3U content of
-- >  Left err -> fail $ "failed to parse playlist: " ++ err
-- >  Right x  -> return x
parsePlaylist :: Format -> ByteString -> Either String Playlist
parsePlaylist M3U = Atto.parseOnly M3U.parsePlaylist
parsePlaylist PLS = Atto.parseOnly PLS.parsePlaylist

--------------------------------------------------------------------------------
-- | Generate a lazy @ByteString@ containing playlist data from the
-- given playlist and in the given format.
--
-- > BL.putStr $ generatePlaylist M3U somePlaylist
generatePlaylist :: Format -> Playlist -> BL.ByteString
generatePlaylist M3U = BL.toLazyByteString . M3U.writePlaylist
generatePlaylist PLS = BL.toLazyByteString . PLS.writePlaylist

--------------------------------------------------------------------------------
-- | Try to figure out a file's format from it's file extension.
--
-- >>> fileNameToFormat "foo.m3u"
-- Just M3U
--
-- >>> fileNameToFormat "foo.txt"
-- Nothing
fileNameToFormat :: FilePath -> Maybe Format
fileNameToFormat ext = case takeExtension ext of
  ".m3u"  -> Just M3U
  ".m3u8" -> Just M3U
  ".pls"  -> Just PLS
  _       -> Nothing

--------------------------------------------------------------------------------
-- | Given a file name that does not have a file extension, return a
-- file name with the appropriate extension included based on the
-- given format.
--
-- >>> appendExtension M3U "foo"
-- "foo.m3u"
appendExtension :: Format -> FilePath -> FilePath
appendExtension M3U = (++ ".m3u")
appendExtension PLS = (++ ".pls")
