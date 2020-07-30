{-# LANGUAGE OverloadedStrings #-}

{-

This file is part of the Haskell package playlists. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/playlists/LICENSE. No part
of playlists package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Text.Playlist.PLS.Reader (parsePlaylist) where

--------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad                     (void)
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8  (double, signed)
import           Data.ByteString                   (ByteString)
import           Data.Text                         (Text)
import           Data.Text.Encoding                (decodeUtf8)
import           Data.Word8                        (isDigit)
import           Text.Playlist.Internal.Attoparsec
import           Text.Playlist.Types

--------------------------------------------------------------------------------
-- | A parser that will process an entire playlist.
parsePlaylist :: Parser Playlist
parsePlaylist = do
  parseHeader
  ts <- many1 parseTrack
  skipMany skipUnusedLine
  return (Playlist (ChunkTrack <$> ts) Nothing)

--------------------------------------------------------------------------------
-- | A pls header will at least contain the "[playlist]" bit but some
-- files also include the lines you'd expect in the footer too.
parseHeader :: Parser ()
parseHeader = do
  skipSpace >> string "[playlist]" >> skipSpace
  skipMany skipUnusedLine

--------------------------------------------------------------------------------
-- | Parse a single track.  Tracks begin with "FileN" where N is a
-- digit.  They are followed by an optional title and optional length.
parseTrack :: Parser Track
parseTrack = do
  (n, url) <- parseFileN
  title    <- (Just <$> parseTitle n) <|> return Nothing

  -- Parse track length.
  mlen <- (Just . realToFrac <$> (skipSpace >> string "Length" >> skipWhile isDigit >> string "=" >> signed double))
      <|> return Nothing

  return Track { trackURL      = url
               , trackTitle    = title
               , trackDuration = mlen
               , trackMeta     = Nothing -- SCTE tags are not expected in PLS playlist.
               }

--------------------------------------------------------------------------------
-- | Skip any line that isn't part of a track.
skipUnusedLine :: Parser ()
skipUnusedLine =
  (string "numberofentries" <|>
   string "NumberOfEntries" <|>
   string "version"         <|>
   string "Version") >> skipLine

--------------------------------------------------------------------------------
-- | Parser for the "FileN" line that contains the track number and
-- URL for the track.  The result is a pair where the first member is
-- the track number and the second member is the URL.
parseFileN :: Parser (ByteString, Text)
parseFileN = do
  skipSpace
  n <- string "File" >> takeWhile1 isDigit
  skipEq
  url <- takeWhile1 (not . isEOL)
  return (n, decodeUtf8 url)

--------------------------------------------------------------------------------
-- | Parser for the title line with the given track number.
parseTitle :: ByteString -> Parser Text
parseTitle n = do
  skipSpace
  void (string "Title" >> string n)
  skipEq
  decodeUtf8 <$> takeWhile1 (not . isEOL)
