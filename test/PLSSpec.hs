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
