# mpv-rs
A libmpv abstraction written in rust that's easy to use and provides the ability to read next to all video and audio codecs.

To get started you need libmpv installed on the system, or enable the "build_libmpv" feature-flag.

# Examples
To run an example, execute `cargo run [--release] --example x -- path`, where x is any of:
* events: multi-threaded event enumeration
* protocol: implementation of custom `filereader://` protocol that… reads files
* sdl2: draw onto an sdl2 framebuffer, keybindings
`path` should be a valid (local if using protocol) path, e.g. music, or a youtube video.

# Dependencies
Libmpv is required, and in the case of the last example sdl2.
However, the feature-flag `build_libmpv` automatically downloads and links against a compatible libmpv.
`static_libmpv` automatically builds and links against a static libmpv. This works cross-platform, but not across architectures.

# Contributing
All pull requests/issues welcome.
