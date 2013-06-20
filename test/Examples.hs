{-

This file is part of the Haskell package playlists. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/playlists/LICENSE. No part
of playlists package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Examples
       ( secretAgent
       , pigRadio
       , utf8Radio
       , hitParty
       , aaFile
       , abFile
       ) where

--------------------------------------------------------------------------------
import Text.Playlist

--------------------------------------------------------------------------------
secretAgent :: Playlist
secretAgent =
  [ Track { trackURL   = "http://mp2.somafm.com:9016"
          , trackTitle = Just "SomaFM: Secret Agent (#1 128k mp3): The soundtrack for your stylish, mysterious, dangerous life. For Spies and PIs too!"
          }

  , Track { trackURL   = "http://mp3.somafm.com:443"
          , trackTitle = Just "SomaFM: Secret Agent (#2 128k mp3): The soundtrack for your stylish, mysterious, dangerous life. For Spies and PIs too!"
          }

  , Track { trackURL   = "http://ice.somafm.com/secretagent"
          , trackTitle = Just "SomaFM: Secret Agent (Firewall-friendly 128k mp3) The soundtrack for your stylish, mysterious, dangerous life. For Spies and PIs too!"
          }
  ]

--------------------------------------------------------------------------------
pigRadio :: Playlist
pigRadio =
  [ Track { trackURL   = "http://s6.mediastreaming.it:8080"
          , trackTitle = Just "Pig Radio - Devoted in Playing the Best Indie Pop/Rock & Electronic. 24/7"
          }

  , Track { trackURL   = "http://s1.viastreaming.net:7480"
          , trackTitle = Just "Pig Radio - Devoted in Playing the Best Indie Pop/Rock & Electronic. 24/7"
          }
  ]

--------------------------------------------------------------------------------
utf8Radio :: Playlist
utf8Radio =
  [ Track { trackURL   = "http://fake.com"
          , trackTitle = Just "Alle otto i bambini erano già in costume da bagno"
          }
  ]

--------------------------------------------------------------------------------
hitParty :: Playlist
hitParty =
  [ Track { trackURL   = "http://firewall.hitparty.net:443"
          , trackTitle = Just "(#1 - Generique) HITPARTY HIT - Pas de pub Que du HIT - Only Hits"
          }
  , Track { trackURL   = "http://icecast.pulsradio.com:80/hitpartyHD.mp3.m3u"
          , trackTitle = Just "(#1 - Generique) HITPARTY HIT - Pas de pub Que du HIT - Only Hits"
          }
  ]

--------------------------------------------------------------------------------
aaFile :: Playlist
aaFile =
  [ Track { trackURL = "http://foo.com", trackTitle = Nothing}
  , Track { trackURL = "http://bar.com", trackTitle = Nothing}
  ]

--------------------------------------------------------------------------------
abFile :: Playlist
abFile = aaFile
