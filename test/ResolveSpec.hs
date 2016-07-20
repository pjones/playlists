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
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import Helper (playlistFromFile)
import System.FilePath (takeFileName)
import Test.Hspec

--------------------------------------------------------------------------------
import Text.Playlist

--------------------------------------------------------------------------------
spec :: Spec
spec = do
  describe "Simple" $
    it "Nested" $ loadAndResolve M3U "nested" `shouldReturn` resolved

--------------------------------------------------------------------------------
loadAndResolve :: Format -> FilePath -> IO Playlist
loadAndResolve format file = do
    playlist <- playlistFromFile format file
    resolve playlist download
  where
    download url = ByteString.readFile (path url)
    path url     = "test/" ++ (takeFileName $ Text.unpack url)

--------------------------------------------------------------------------------
resolved :: Playlist
resolved = [ Track "http://foo.com"             Nothing
           , Track "http://bar.com"             Nothing
           , Track "http://foo.com/aa.baz.live" Nothing
           ]
