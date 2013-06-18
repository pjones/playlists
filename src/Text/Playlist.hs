--------------------------------------------------------------------------------
module Text.Playlist
       ( Track (..)
       , Playlist
       , Format (..)
       , parsePlaylist
       ) where

--------------------------------------------------------------------------------
import qualified Data.Attoparsec.ByteString as Atto
import Data.ByteString (ByteString)
import Text.Playlist.Types
import qualified Text.Playlist.PLS.Reader as PLS

--------------------------------------------------------------------------------
parsePlaylist :: Format -> ByteString -> Either String Playlist
parsePlaylist PLS x = Atto.parseOnly PLS.parsePlaylist x
