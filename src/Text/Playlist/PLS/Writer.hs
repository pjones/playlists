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
module Text.Playlist.PLS.Writer
       ( writePlaylist
       , writeTrack
       ) where

--------------------------------------------------------------------------------
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import Data.Text.Encoding (encodeUtf8)
import Text.Playlist.Types

--------------------------------------------------------------------------------
writePlaylist :: Playlist -> Builder
writePlaylist x =
  B.byteString "[playlist]\n"                  <>
  mconcat (zipWith (curry writeTrack) x [1..]) <>
  B.byteString "NumberOfEntries="              <>
  B.stringUtf8 (show . length $ x)             <>
  B.byteString "\nVersion=2\n"

--------------------------------------------------------------------------------
writeTrack :: (Track, Int) -> Builder
writeTrack x = writeFileN x <> writeTitle x <> writeLength x

--------------------------------------------------------------------------------
writeFileN :: (Track, Int) -> Builder
writeFileN (x, n) =
  B.byteString "File"                      <>
  B.stringUtf8 (show n)                    <>
  B.charUtf8 '='                           <>
  B.byteString (encodeUtf8 . trackURL $ x) <>
  B.charUtf8 '\n'

--------------------------------------------------------------------------------
writeTitle :: (Track, Int) -> Builder
writeTitle (x, n) =
  case trackTitle x of
    Nothing -> mempty
    Just y  -> B.byteString "Title"        <>
               B.stringUtf8 (show n)       <>
               B.charUtf8 '='              <>
               B.byteString (encodeUtf8 y) <>
               B.charUtf8 '\n'

--------------------------------------------------------------------------------
writeLength :: (Track, Int) -> Builder
writeLength (x, n) =
  case trackDuration x of
    Nothing -> mempty
    Just l  -> B.byteString "Length" <>
               B.stringUtf8 (show n) <>
               B.charUtf8 '='        <>
               B.stringUtf8 (show l) <>
               B.charUtf8 '\n'
