{-

This file is part of the Haskell package playlists. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/playlists/LICENSE. No part
of playlists package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module PLSSpec (spec) where

--------------------------------------------------------------------------------
import Examples (secretAgent, pigRadio, utf8Radio)
import Helper (playlistFromFile, roundTrip)
import Test.Hspec
import Text.Playlist

--------------------------------------------------------------------------------
spec :: Spec
spec = do
  describe "Parsing" $ do
    it "Secret Agent" $ playlistFromFile' "sa"   `shouldReturn` secretAgent
    it "Pig Radio"    $ playlistFromFile' "pig"  `shouldReturn` pigRadio
    it "UTF8 Radio"   $ playlistFromFile' "utf8" `shouldReturn` utf8Radio
  describe "Generating" $ do
    it "Secret Agent" $ roundTrip' secretAgent `shouldReturn` secretAgent
    it "Pig Radio"    $ roundTrip' pigRadio    `shouldReturn` pigRadio
    it "UTF8 Radio"   $ roundTrip' utf8Radio   `shouldReturn` utf8Radio

--------------------------------------------------------------------------------
playlistFromFile' :: FilePath -> IO Playlist
playlistFromFile' = playlistFromFile PLS

--------------------------------------------------------------------------------
roundTrip' :: Playlist -> IO Playlist
roundTrip' = roundTrip PLS
