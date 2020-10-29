# Release History

## 0.5 (December 1, 2017)

  * Add support for track length (thanks to Catherine Galkina)

  * Widen dependency versions to latest available.

  * Minor updates:

    - 0.5.1 (October 29, 2020): Update dependencies

## 0.4 (November 21, 2016)

  * Added the `resolve` function.  If you have a playlist that
    contains references to remote playlists, this new function will
    recursively download and process playlists until it has a flat
    playlist with no further remote references.

    See the `playlists-http` package for an example of using the
    `resolve` function.

  * Added the `parserForFormat` function which directly exposes the
    `attoparsec` parser for each playlist type.  This should make it
    easy to write space efficient functions that parse playlists.

  * Minor updates:

    - 0.4.1 (February 5, 2017): Update dependencies

## 0.3 (January 24, 2015)

  * Update dependencies to recent versions (thanks to Wieland Hoffmann).
