{-

This file is part of the Haskell package playlists. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/playlists/LICENSE. No part
of playlists package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Text.Playlist.Internal.ReadWrite
       ( parserForFormat
       , parsePlaylist
       , generatePlaylist
       ) where

--------------------------------------------------------------------------------
import qualified Data.Attoparsec.ByteString as Atto
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BL

--------------------------------------------------------------------------------
import qualified Text.Playlist.M3U.Reader as M3U
import qualified Text.Playlist.M3U.Writer as M3U
import qualified Text.Playlist.PLS.Reader as PLS
import qualified Text.Playlist.PLS.Writer as PLS
import Text.Playlist.Types

--------------------------------------------------------------------------------
-- | Return the appropriate attoparsec parser for the given playlist format.
parserForFormat :: Format -> Atto.Parser Playlist
parserForFormat M3U = M3U.parsePlaylist
parserForFormat PLS = PLS.parsePlaylist

--------------------------------------------------------------------------------
-- | Parse a playlist from a @ByteString@.  Parsing may fail in which
-- case an error message is returned in @Left@.
--
-- > content <- BS.getContents
-- > case parsePlaylist M3U content of
-- >  Left err -> fail $ "failed to parse playlist: " ++ err
-- >  Right x  -> return x
parsePlaylist :: Format -> ByteString -> Either String Playlist
parsePlaylist f = Atto.parseOnly (parserForFormat f)

--------------------------------------------------------------------------------
-- | Generate a lazy @ByteString@ containing playlist data from the
-- given playlist and in the given format.
--
-- > BL.putStr $ generatePlaylist M3U somePlaylist
generatePlaylist :: Format -> Playlist -> BL.ByteString
generatePlaylist M3U = BL.toLazyByteString . M3U.writePlaylist
generatePlaylist PLS = BL.toLazyByteString . PLS.writePlaylist
