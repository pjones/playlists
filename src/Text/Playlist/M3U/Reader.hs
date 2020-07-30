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
import           Control.Monad                     (unless)
import           Data.Attoparsec.ByteString        (Parser)
import qualified Data.Attoparsec.ByteString        as Atto
import qualified Data.Attoparsec.ByteString.Char8  as Atto8
import           Data.Char                         (ord)
import           Data.Maybe                        (catMaybes)
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import           Data.Text.Encoding                (decodeUtf8)
import           Text.Playlist.Internal.Attoparsec
import           Text.Playlist.Types

parsePlaylist :: Parser Playlist
parsePlaylist = playlistParser

playlistParser :: Parser Playlist
playlistParser = do
  optional ("#EXTM3U" *> Atto8.skipSpace)
  playlistGlobalTags <- globalTags
  playlistTracks <- many trackParser
  return Playlist{..}

tags :: (Tag -> Bool) -> Parser [Tag]
tags p = catMaybes <$> many (tagOrComment p)

anyTags :: Parser [Tag]
anyTags = tags (const True)

globalTags :: Parser [Tag]
globalTags = tags isGlobal
  where
    isGlobal Tag{..} = tagName `elem`
      [ "#EXTM3U"
      , "#EXT-X-VERSION"
      , "#EXT-X-TARGETDURATION"
      , "#EXT-X-MEDIA-SEQUENCE"
      ]

tagOrComment :: (Tag -> Bool) -> Parser (Maybe Tag)
tagOrComment p = do
  mTag <- "#" *> (("EXT" *> fmap Just tagParser) <|> comment)
  unless (all p mTag) $
    fail "Tag does not satisfy the predicate"
  return mTag
  where
    comment = Atto.takeTill isEOL *> Atto8.skipSpace *> pure Nothing

tagParser :: Parser Tag
tagParser = do
  -- #EXT
  tagStr <- decodeUtf8 <$> Atto.takeTill isEOL
  let (name, val) = Text.break (== ':') tagStr
      tagName = TagName ("#EXT" <> name)
      tagValue = Text.drop 1 val
      tag = Tag{..}
  Atto8.skipSpace
  return tag

trackParser :: Parser Track
trackParser = do
  trackTags <- anyTags
  trackURL <- parseURL
  let parsedEXTINF = parseEXTINF trackTags
      trackDuration = fst <$> parsedEXTINF
      trackTitle = snd <$> parsedEXTINF
      trackDateTime = parseEXT_X_PROGRAM_DATETIME trackTags
  return Track{..}

--------------------------------------------------------------------------------
-- | Parser for URL or file name in a M3U file.  The URL is the entire
-- line so this parser extracts the entire line and decodes it.
--
-- IMPORTANT: parseURL does not accept an empty line
parseURL :: Parser Text
parseURL = Text.pack <$> Atto8.many1 (Atto8.satisfy (not . Atto8.isEndOfLine . fromIntegral . ord)) <* Atto8.skipSpace

