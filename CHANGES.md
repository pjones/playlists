# Version 0.4.0.0 (November 21, 2016)

  * Added the `resolve` function.  If you have a playlist that
    contains references to remote playlists, this new function will
    recursively download and process playlists until it has a flat
    playlist with no further remote references.

    See the `playlists-http` package for an example of using the
    `resolve` function.

  * Added the `parserForFormat` function which directly exposes the
    `attoparsec` parser for each playlist type.  This should make it
    easy to write space efficient functions that parse playlists.

# Version 0.3.0.0 (January 24, 2015)

  * Update dependencies to recent versions (thanks to Wieland Hoffmann).
