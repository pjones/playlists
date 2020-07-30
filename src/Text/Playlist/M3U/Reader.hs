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
module Text.Playlist.M3U.Reader (parsePlaylist) where

--------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad                     (void)
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8  (decimal, double, signed)
import           Data.Maybe                        (catMaybes)
import           Data.Text                         (Text)
import           Data.Text.Encoding                (decodeUtf8)
import           Text.Playlist.Internal.Attoparsec
import           Text.Playlist.Types
import           Text.Read                         (readMaybe)

--------------------------------------------------------------------------------
data Directive = TrackProgramDateTime | TrackDuration | Comment | Meta

-- | Parser for a complete M3U playlist.
parsePlaylist :: Parser Playlist
parsePlaylist = do
  mmediaSequence' <- many' parseMediaSequence
  let mmediaSequence = case catMaybes mmediaSequence' of
        []    -> Nothing
        x : _ -> pure x
  cs <- many1 parseChunks
  void (many' commentOrDirective) -- Trailing comments.
  return (Playlist cs mmediaSequence)

parseMediaSequence :: Parser (Maybe Text)
parseMediaSequence = do
  skipSpace
  skip (== 35) -- Comment character '#'
  isSequence <- (string "EXT-X-MEDIA-SEQUENCE:" >> pure True) <|> pure False
  if isSequence
    then do
      mtext <- Just . decodeUtf8 <$> takeWhile1 (not . isEOL)
      pure mtext
    else pure Nothing

parseChunks :: Parser Chunk
parseChunks = ChunkTrack <$> parseTrack

--------------------------------------------------------------------------------
-- | Parser for a single track in a M3U file.
parseTrack :: Parser Track
parseTrack = do
  -- Get the length and title closest to the URL or Nothing.
  (title, len) <- maybeTitleAndLength . reverse <$> (many' commentOrDirective)
  url   <- parseURL
  return Track { trackURL      = url
               , trackTitle    = title
               , trackDuration = len
               , trackMeta     = Nothing
               , trackTime     = Nothing
               }
    where
      maybeTitleAndLength lst =
        case catMaybes lst of
          []    -> (Nothing, Nothing)
          x : _ -> x
--------------------------------------------------------------------------------
-- | Parser for URL or file name in a M3U file.  The URL is the entire
-- line so this parser extracts the entire line and decodes it.
parseURL :: Parser Text
parseURL = decodeUtf8 <$> takeWhile1 (not . isEOL) <* skipSpace

--------------------------------------------------------------------------------
-- | Comment parser with a twist.  In the extended M3U format metadata
-- for a track can be placed in a comment that appears just before the
-- URL.  This parser succeeds if the current line is a comment, and
-- always skips over the entire comment.  If the comment represents an
-- EXTINF directive then that information will be returned in a @Just@.
-- If it's just a regular comment then @Nothing@ is returned.
commentOrDirective :: Parser (Maybe (Maybe Text, Maybe Float))
commentOrDirective = do
  skipSpace
  skip (== 35) -- Comment character "#"
  isDirective <- (string "EXTINF:" >> return True)
    <|> (string "EXT-X-PROGRAM-DATE-TIME:" >> return True)
    <|> return False
  if isDirective then directive <|> comment else comment
    where
      comment   = skipLine >> return Nothing
      directive = do
        mlen <- (Just . realToFrac <$> signed double) <|> return Nothing -- Parse length.
        skip (== 44)                                                     -- Skip comma.
        mtext <- (Just . decodeUtf8 <$> takeWhile1 (not . isEOL)) <|> (return Nothing)
        skipLine
        return (Just (mtext, mlen))

