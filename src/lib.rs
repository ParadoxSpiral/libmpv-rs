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

//! This crate provides abstractions for
//! [libmpv](https://github.com/mpv-player/mpv/tree/master/libmpv) of the
//! [mpv media player](https://github.com/mpv-player/mpv).
//!
//! Libmpv requires `LC_NUMERIC` to be `C`, which should be the default value.
//!
//! Most of the documentation is paraphrased or even copied from the
//! [mpv manual](https://mpv.io/manual/master/),
//! if any questions arise it will probably answer them in much more depth than this documentation.
//!
//! # Examples
//!
//! See the 'examples' directory in the crate root.

// TODO: clean up docs in general
// TODO: Add test for every single function

// Procedure for updating to new libmpv:
// - make any nessecary API change (if so, bump crate version)
// - update MPV_CLIENT_API consts in lib.rs
// - update constants in build.rs
// - run tests and examples to test whether they still work

extern crate mpv_sys as raw;
extern crate parking_lot;
#[macro_use]
extern crate error_chain;

use std::os::raw as ctype;

/// All Event ids, not all APIs might use all ids.
pub use raw::mpv_event_id as EventId;
/// Verbosity level of log events.
pub use raw::mpv_log_level as LogLevel;
/// The reason an `Event::EndFile` was fired.
pub use raw::mpv_end_file_reason as EndFileReason;
/// An mpv error, used in `Error::Native`
pub use raw::mpv_error as MpvError;
mod wrapper;
pub use wrapper::*;
#[cfg(test)]
mod tests;

#[allow(missing_docs)]
pub const MPV_CLIENT_API_MAJOR: ctype::c_ulong = 1;
#[allow(missing_docs)]
pub const MPV_CLIENT_API_MINOR: ctype::c_ulong = 25;
#[allow(missing_docs)]
pub const MPV_CLIENT_API_VERSION: ctype::c_ulong =
    { MPV_CLIENT_API_MAJOR << 16 | MPV_CLIENT_API_MINOR };
