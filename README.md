# Haskell Playlists Library and Tool

[![CI](https://github.com/pjones/playlists/workflows/CI/badge.svg)](https://github.com/pjones/playlists/actions)
[![GitHub tag (latest by date)](https://img.shields.io/github/v/tag/pjones/playlists?label=release)](https://github.com/pjones/playlists/releases)
[![Hackage](https://img.shields.io/hackage/v/playlists)](https://hackage.haskell.org/package/playlists)

Playlists is a library for working with media playlist files.  The
original motivation for the library was extracting URLs for streaming
radio stations that use PLS and M3U playlist files.

The package also includes an executable that can dump the URLs from a
playlist file and convert between playlist file formats.

## Supported Formats

  * [PLS][]
  * [M3U and M3U8][M3U]

## Future Plans

Some playlist files can be really big.  I plan on adding support for
incremental parsing and generating via `pipes` at some point in the
future.

## Library Example

```haskell
import qualified Data.ByteString as BS
import Text.Playlist

readPlaylist :: Format -> IO Playlist
readPlaylist fmt = do
  content <- BS.getContents
  case parsePlaylist fmt content of
    Left err -> fail $ "failed to parse playlist on stdin: " ++ err
    Right x  -> return x
```

## Executable Example

    $ playlist urls --format PLS < somefile.pls

    $ playlist convert --from PLS --to M3U < somefile.pls

[pls]: http://en.wikipedia.org/wiki/PLS_(file_format)
[m3u]: http://en.wikipedia.org/wiki/M3U
