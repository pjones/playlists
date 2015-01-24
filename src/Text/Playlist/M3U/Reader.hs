{-

This file is part of the Haskell package playlists. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/playlists/LICENSE. No part
of playlists package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Text.Playlist.M3U.Reader (parsePlaylist) where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad (msum, void)
import Data.Attoparsec.ByteString
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Word8 (isDigit)
import Text.Playlist.Internal.Attoparsec
import Text.Playlist.Types

--------------------------------------------------------------------------------
-- | Parser for a complete M3U playlist.
parsePlaylist :: Parser Playlist
parsePlaylist = do
  ts <- many1 parseTrack
  void (many' commentOrTitle) -- Trailing comments.
  return ts

--------------------------------------------------------------------------------
-- | Parser for a single track in a M3U file.
parseTrack :: Parser Track
parseTrack = do
  -- Get the title closest to the URL or Nothing.
  title <- msum . reverse <$> many' commentOrTitle
  url   <- parseURL

  return Track { trackURL   = url
               , trackTitle = title
               }

--------------------------------------------------------------------------------
-- | Parser for URL or file name in a M3U file.  The URL is the entire
-- line so this parser extracts the entire line and decodes it.
parseURL :: Parser Text
parseURL = decodeUtf8 <$> takeWhile1 (not . isEOL) <* skipSpace

--------------------------------------------------------------------------------
-- | Comment parser with a twist.  In the extended M3U format metadata
-- for a track can be placed in a comment that appears just before the
-- URL.  This parser succeeds if the current line is a comment, and
-- always skips over the entire comment.  If the comment represents a
-- track title then that information will be returned in a @Just@.  If
-- it's just a regular comment then @Nothing@ is returned.
commentOrTitle :: Parser (Maybe Text)
commentOrTitle = do
    skipSpace
    skip (== 35) -- Comment character "#"
    istitle <- (string "EXTINF:" >> return True) <|> return False
    if istitle then title <|> comment else comment
  where
    comment = skipLine >> return Nothing
    title   = do skip (== 45) <|> return () -- Skip optional negative sign.
                 skipWhile isDigit          -- Skip length.
                 skip (== 44)               -- Skip comma.
                 skipSpace
                 text <- decodeUtf8 <$> takeWhile1 (not . isEOL)
                 skipSpace
                 return (Just text)
