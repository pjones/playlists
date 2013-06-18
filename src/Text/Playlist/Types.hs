--------------------------------------------------------------------------------
module Text.Playlist.Types
       ( Track (..)
       , Playlist
       , Format (..)
       ) where

--------------------------------------------------------------------------------
import Data.Text (Text)

--------------------------------------------------------------------------------
-- | A single music file or streaming URL.
data Track = Track
  { trackURL   :: Text       -- ^ URL for a file or streaming resource.
  , trackTitle :: Maybe Text -- ^ Optional title.
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- | A list of 'Track's.
type Playlist = [Track]

--------------------------------------------------------------------------------
-- | Playlist formats.                
data Format = PLS               -- ^ http://en.wikipedia.org/wiki/PLS_(file_format)
              deriving (Show, Eq)
