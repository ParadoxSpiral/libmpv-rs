// Copyright (C) 2016  ParadoxSpiral
//
// This file is part of mpv-rs.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

#![deny(missing_docs)]
#![allow(unknown_lints)]
#![feature(core_intrinsics, pub_restricted, untagged_unions, heap_api, alloc)]

//! This crate provides abstractions for (libmpv)[https://github.com/mpv-player/mpv/tree/master/libmpv] of the (mpv media player)[https://github.com/mpv-player/mpv].
//!
//! Libmpv requires `LC_NUMERIC` to be `C`. This is set once when the first parent is created.
//! Do not change this during the usage of this crate.
//!
//! Most of the documentation is paraphrased or even copied from the (mpv manual)[https://mpv.io/manual/master/],
//! if any questions arise it will probably answer them in much more depth than this documentation.
//!
//! # Examples
//!
//! See the 'examples' directory in the crate root.

// TODO: write an overview of examples
// TODO: write docs for build/static_libmpv
// TODO: clean up docs in general
// TODO: Once const-dependant type system lands, remove event feature by const bool
// TODO: Add test for every single function

// Procedure for updating to new libmpv:
// - make any nessecary API change (if so, bump crate version)
// - update MPV_CLIENT_API consts in lib.rs
// - update constants in build.rs
// - run tests and examples to test whether they still work

extern crate alloc;
extern crate libc;
extern crate parking_lot;
#[macro_use]
extern crate enum_primitive;
#[macro_use]
extern crate error_chain;

/// Contains bindings to libmpv functions and data structures.
pub mod raw;
pub use raw::MpvLogLevel as LogLevel;
pub use raw::MpvEndFileReason as EndFileReason;
mod wrapper;
pub use wrapper::*;
#[cfg(test)]
mod tests;

#[allow(missing_docs)]
pub const MPV_CLIENT_API_MAJOR: u32 = 1;
#[allow(missing_docs)]
pub const MPV_CLIENT_API_MINOR: u32 = 24;
#[allow(missing_docs)]
pub const MPV_CLIENT_API_VERSION: u32 = { MPV_CLIENT_API_MAJOR << 16 | MPV_CLIENT_API_MINOR };
