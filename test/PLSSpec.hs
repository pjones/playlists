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
import qualified Data.ByteString as BS
import Examples (secretAgent, pigRadio)
import Test.Hspec
import Text.Playlist

--------------------------------------------------------------------------------
spec :: Spec
spec = do
  describe "Parsing" $ do
    it "Secret Agent" $ playlistFromFile "sa"  `shouldReturn` secretAgent
    it "Pig Radio"    $ playlistFromFile "pig" `shouldReturn` pigRadio

--------------------------------------------------------------------------------
playlistFromFile :: FilePath -> IO Playlist
playlistFromFile file = do
    content <- BS.readFile file'
    case parsePlaylist PLS content of
      Left err   -> fail $ "failed to parse: " ++ file' ++ ": " ++ err
      Right plst -> return plst
  where file' = "test/" ++ file ++ ".pls"
