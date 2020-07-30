{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
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
module Text.Playlist.M3U.Reader where

--------------------------------------------------------------------------------

import           Control.Applicative
import           Control.Monad                     (void)
import           Data.Attoparsec.ByteString        (Parser)
import qualified Data.Attoparsec.ByteString        as Atto
import qualified Data.Attoparsec.ByteString.Char8  as Atto8
import           Data.Char                         (ord)
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import           Data.Text.Encoding                (decodeUtf8)
import           Text.Playlist.Internal.Attoparsec
import           Text.Playlist.Types

parsePlaylist :: Parser Playlist
parsePlaylist = playlistParser

playlistParser :: Parser Playlist
playlistParser = do
  playlistGlobalTags <- globalTagsParser
  playlistTracks <- many trackParser
  return Playlist{..}

globalTagsParser :: Parser [Tag]
globalTagsParser = do
  "#EXTM3U" *> Atto8.skipSpace
  skipComments
  -- TODO: collect MEDIA-SEQUENCE and other global
  return []

skipComments :: Parser ()
skipComments = return () -- void (many comment)

comment :: Parser ()
comment = "#" *> notEXT
  where
    notEXT = ("EXT" *> fail "")
         <|> (Atto.takeTill isEOL *> Atto8.skipSpace)

tagParser :: Parser Tag
tagParser = skipComments *> do
  tagStr <- decodeUtf8 <$> ("#EXT" *> Atto.takeTill isEOL)
  let (name, val) = Text.break (== ':') tagStr
      tagName = TagName ("#EXT" <> name)
      tagValue = Text.drop 1 val
  Atto8.skipSpace
  return Tag{..}
  where
    ord8 = fromIntegral . ord

trackParser :: Parser Track
trackParser = do
  trackTags <- many tagParser
--   case lookupTag "#EXTINF" trackTags of
--     Tag { tagValue = durationAndTitle } -> do
--       return
--
--       -- TODO: parse durationAndTitle
  trackURL <- parseURL
  return Track{..}

--------------------------------------------------------------------------------
-- | Parser for URL or file name in a M3U file.  The URL is the entire
-- line so this parser extracts the entire line and decodes it.
parseURL :: Parser Text
parseURL = Text.pack <$> Atto8.many1 (Atto8.satisfy (not . Atto8.isEndOfLine . fromIntegral . ord)) <* Atto8.skipSpace

