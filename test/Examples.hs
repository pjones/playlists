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
module Examples
       ( secretAgent
       , pigRadio
       , utf8Radio
       , hitParty
       , aaFile
       , abFile
       ) where

--------------------------------------------------------------------------------
import           Text.Playlist

--------------------------------------------------------------------------------
secretAgent :: Playlist
secretAgent = Playlist []
  [ Track { trackURL   = "http://mp2.somafm.com:9016"
          , trackTitle = Just "SomaFM: Secret Agent (#1 128k mp3): The soundtrack for your stylish, mysterious, dangerous life. For Spies and PIs too!"
          , trackDuration = Just (-1)
          , trackDateTime = Nothing
          , trackTags = []
          }

  , Track { trackURL   = "http://mp3.somafm.com:443"
          , trackTitle = Just "SomaFM: Secret Agent (#2 128k mp3): The soundtrack for your stylish, mysterious, dangerous life. For Spies and PIs too!"
          , trackDuration = Just (-1)
          , trackDateTime = Nothing
          , trackTags = []
          }

  , Track { trackURL   = "http://ice.somafm.com/secretagent"
          , trackTitle = Just "SomaFM: Secret Agent (Firewall-friendly 128k mp3) The soundtrack for your stylish, mysterious, dangerous life. For Spies and PIs too!"
          , trackDuration = Just (-1)
          , trackDateTime = Nothing
          , trackTags = []
          }
  ]

--------------------------------------------------------------------------------
pigRadio :: Playlist
pigRadio = Playlist []
  [ Track { trackURL   = "http://s6.mediastreaming.it:8080"
          , trackTitle = Just "Pig Radio - Devoted in Playing the Best Indie Pop/Rock & Electronic. 24/7"
          , trackDuration = Just 0
          , trackDateTime = Nothing
          , trackTags = []
          }

  , Track { trackURL   = "http://s1.viastreaming.net:7480"
          , trackTitle = Just "Pig Radio - Devoted in Playing the Best Indie Pop/Rock & Electronic. 24/7"
          , trackDuration = Just (-1)
          , trackDateTime = Nothing
          , trackTags = []
          }
  ]

--------------------------------------------------------------------------------
utf8Radio :: Playlist
utf8Radio = Playlist []
  [ Track { trackURL   = "http://fake.com"
          , trackTitle = Just "Alle otto i bambini erano già in costume da bagno"
          , trackDuration = Nothing
          , trackDateTime = Nothing
          , trackTags = []
          }
  ]

--------------------------------------------------------------------------------
hitParty :: Playlist
hitParty = Playlist []
  [ Track { trackURL   = "http://firewall.hitparty.net:443"
          , trackTitle = Just "(#1 - Generique) HITPARTY HIT - Pas de pub Que du HIT - Only Hits"
          , trackDuration = Just (-1)
          , trackDateTime = Nothing
          , trackTags = [Tag {tagName = "#EXTINF", tagValue = "-1.0,(#1 - Generique) HITPARTY HIT - Pas de pub Que du HIT - Only Hits"}]
          }
  , Track { trackURL   = "http://icecast.pulsradio.com:80/hitpartyHD.mp3.m3u"
          , trackTitle = Just "(#1 - Generique) HITPARTY HIT - Pas de pub Que du HIT - Only Hits"
          , trackDuration = Just (-1)
          , trackDateTime = Nothing
          , trackTags = [Tag {tagName = "#EXTINF", tagValue = "-1.0,(#1 - Generique) HITPARTY HIT - Pas de pub Que du HIT - Only Hits"}]
          }
  ]

--------------------------------------------------------------------------------
aaFile :: Playlist
aaFile = Playlist []
  [ Track { trackURL = "http://foo.com"
          , trackTitle = Nothing
          , trackDuration = Nothing
          , trackDateTime = Nothing
          , trackTags = []
          }
  , Track { trackURL = "http://bar.com"
          , trackTitle = Nothing
          , trackDuration = Nothing
          , trackDateTime = Nothing
          , trackTags = []
          }
  ]

--------------------------------------------------------------------------------
abFile :: Playlist
abFile = aaFile
