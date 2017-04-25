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
module ResolveSpec (spec) where

--------------------------------------------------------------------------------
import Data.Text (Text)
import qualified Data.Text as Text
import Helper (playlistFromFile)
import System.FilePath (dropExtension, takeFileName)
import Test.Hspec

--------------------------------------------------------------------------------
import Text.Playlist

--------------------------------------------------------------------------------
spec :: Spec
spec =
  describe "Simple" $
    it "Nested" $ loadAndResolve M3U "nested" `shouldReturn` resolved

--------------------------------------------------------------------------------
loadAndResolve :: Format -> FilePath -> IO Playlist
loadAndResolve format file = do
    playlist <- playlistFromFile format file
    resolve playlist download
  where
    download :: Text -> IO Playlist
    download = playlistFromFile format . dropExtension .
               takeFileName . Text.unpack

--------------------------------------------------------------------------------
resolved :: Playlist
resolved = [ Track "http://foo.com"             Nothing Nothing
           , Track "http://bar.com"             Nothing Nothing
           , Track "http://foo.com/aa.baz.live" Nothing Nothing
           ]
