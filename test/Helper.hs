{-

This file is part of the Haskell package playlists. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/playlists/LICENSE. No part
of playlists package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Helper (playlistFromFile, roundTrip) where

--------------------------------------------------------------------------------
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Text.Playlist

--------------------------------------------------------------------------------
playlistFromFile :: Format -> FilePath -> IO Playlist
playlistFromFile fmt file = do
    content <- BS.readFile file'
    case parsePlaylist fmt content of
      Left err   -> fail $ "failed to parse: " ++ file' ++ ": " ++ err
      Right plst -> return plst
  where file' = "test/" ++ file ++ "." ++ map toLower (show fmt)

--------------------------------------------------------------------------------
roundTrip :: Format -> Playlist -> IO Playlist
roundTrip fmt plst =
  case parsePlaylist fmt $ BL.toStrict $ writePlaylist fmt plst of
    Left err    -> fail $ "failed to roundTrip playlist: " ++ err
    Right plst' -> return plst'
