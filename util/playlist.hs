{-

This file is part of the Haskell package playlists. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/playlists/LICENSE. No part
of playlists package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Main where

--------------------------------------------------------------------------------
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import           Options.Applicative
import           Text.Playlist

--------------------------------------------------------------------------------
data Command = CmdURLs Format | CmdConvert Format Format

--------------------------------------------------------------------------------
urlCommandOptions :: Parser Command
urlCommandOptions = CmdURLs
  <$> option auto (short 'f' <> long "format" <> metavar "FORMAT" <> help "Input format")

--------------------------------------------------------------------------------
convertCommandOptions :: Parser Command
convertCommandOptions = CmdConvert
  <$> option auto (short 'f' <> long "from" <> metavar "FORMAT" <> help "Input format")
  <*> option auto (short 't' <> long "to"   <> metavar "FORMAT" <> help "Output format")

--------------------------------------------------------------------------------
commands :: Parser Command
commands = subparser
    (command "urls"    (info urlCommandOptions     (progDesc urlsDesc)) <>
     command "convert" (info convertCommandOptions (progDesc convertDesc)))
  where
    urlsDesc = "Dump all URLs (or file names) from a playlist"
    convertDesc = "Convert a playlist from one format to another"

--------------------------------------------------------------------------------
readPlaylist :: Format -> IO Playlist
readPlaylist fmt = do
  content <- BS.getContents
  case parsePlaylist fmt content of
    Left err -> fail $ "failed to parse playlist on stdin: " ++ err
    Right x  -> return x

--------------------------------------------------------------------------------
writePlaylist :: Format -> Playlist -> IO ()
writePlaylist fmt plst = BL.putStr $ generatePlaylist fmt plst

--------------------------------------------------------------------------------
dumpURLs :: Format -> IO ()
dumpURLs fmt = mapM_ (putStrLn . T.unpack . trackURL) . tracks =<< readPlaylist fmt

--------------------------------------------------------------------------------
convertPlaylist :: Format -> Format -> IO ()
convertPlaylist fmtA fmtB = readPlaylist fmtA >>= writePlaylist fmtB

--------------------------------------------------------------------------------
main :: IO ()
main = do
  cmd <- execParser $ info (commands <**> helper) idm
  case cmd of
    CmdURLs x      -> dumpURLs x
    CmdConvert x y -> convertPlaylist x y
