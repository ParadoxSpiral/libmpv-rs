# mpv-rs
A libmpv abstraction written in rust that's easy to use and provides the ability to read next to all video and audio codecs.

# Dependencies
Rust version >= 1.25. Libmpv version 1.100 (mpv version 0.29) is the minimum required version.

For ease of building, you can use the `build_libmpv` feature that is used to link against. Especially useful to cross compile to windows.

# Examples
To run an example, execute `cargo run [--release] --example x -- path`, where x is any of:
* `events_complex`: multi-threaded event enumeration
* `events_simple`: single-threaded event enumeration
* `protocol`: implementation of custom `filereader://` protocol that… reads files
* `imgui`: minimalistic GUI using `imgui-rs`
`path` should be a valid (local if using protocol) path, e.g. music, or a youtube video.

# Contributing
All pull requests/issues welcome.
