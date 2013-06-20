{-

This file is part of the Haskell package playlists. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/playlists/LICENSE. No part
of playlists package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module M3USpec (spec) where

--------------------------------------------------------------------------------
import Examples (hitParty, aaFile, abFile)
import Helper (playlistFromFile) -- , roundTrip)
import Test.Hspec
import Text.Playlist

--------------------------------------------------------------------------------
spec :: Spec
spec = do
  describe "Parsing" $ do
    it "HIT Party" $ playlistFromFile' "hp" `shouldReturn` hitParty
    it "File aa"   $ playlistFromFile' "aa" `shouldReturn` aaFile
    it "File ab"   $ playlistFromFile' "ab" `shouldReturn` abFile

--------------------------------------------------------------------------------
playlistFromFile' :: FilePath -> IO Playlist
playlistFromFile' = playlistFromFile M3U
