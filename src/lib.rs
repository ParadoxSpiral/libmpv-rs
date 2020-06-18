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

#![warn(missing_docs)]

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

#[cfg(all(feature = "events_simple", feature = "events_sync"))]
compile_error!(
    "Using events_simple and events_sync at the same time is forbidden, because it \
     will more likely than not result in a lot of hard to debug issues. This will later be \
     relaxed to a per mpv instance const generic flag."
);

// TODO: clean up docs in general

// Procedure for updating to new libmpv:
// - make any nessecary API change (if so, bump crate version)
// - update MPV_CLIENT_API consts in lib.rs
// - update constants in build.rs
// - run tests and examples to test whether they still work

use std::os::raw as ctype;

#[allow(missing_docs)]
pub const MPV_CLIENT_API_MAJOR: ctype::c_ulong = 1;
#[allow(missing_docs)]
pub const MPV_CLIENT_API_MINOR: ctype::c_ulong = 101;
#[allow(missing_docs)]
pub const MPV_CLIENT_API_VERSION: ctype::c_ulong =
    MPV_CLIENT_API_MAJOR << 16 | MPV_CLIENT_API_MINOR;

#[cfg(test)]
mod tests;
mod wrapper;

pub use crate::wrapper::*;

/// A format mpv can use.
pub use mpv_sys::mpv_format as MpvFormat;
pub mod mpv_format {
    #![allow(missing_docs)]
    pub use mpv_sys::mpv_format_MPV_FORMAT_DOUBLE as Double;
    pub use mpv_sys::mpv_format_MPV_FORMAT_FLAG as Flag;
    pub use mpv_sys::mpv_format_MPV_FORMAT_INT64 as Int64;
    pub use mpv_sys::mpv_format_MPV_FORMAT_NONE as None;
    pub use mpv_sys::mpv_format_MPV_FORMAT_OSD_STRING as OsdString;
    pub use mpv_sys::mpv_format_MPV_FORMAT_STRING as String;
}

/// A mpv_sys mpv error.
pub use mpv_sys::mpv_error as MpvError;
pub mod mpv_error {
    #![allow(missing_docs)]
    pub use mpv_sys::mpv_error_MPV_ERROR_AO_INIT_FAILED as AoInitFailed;
    pub use mpv_sys::mpv_error_MPV_ERROR_COMMAND as Command;
    pub use mpv_sys::mpv_error_MPV_ERROR_EVENT_QUEUE_FULL as EventQueueFull;
    pub use mpv_sys::mpv_error_MPV_ERROR_GENERIC as Generic;
    pub use mpv_sys::mpv_error_MPV_ERROR_INVALID_PARAMETER as InvalidParameter;
    pub use mpv_sys::mpv_error_MPV_ERROR_LOADING_FAILED as LoadingFailed;
    pub use mpv_sys::mpv_error_MPV_ERROR_NOMEM as NoMem;
    pub use mpv_sys::mpv_error_MPV_ERROR_NOTHING_TO_PLAY as NothingToPlay;
    pub use mpv_sys::mpv_error_MPV_ERROR_NOT_IMPLEMENTED as NotImplemented;
    pub use mpv_sys::mpv_error_MPV_ERROR_OPTION_ERROR as OptionError;
    pub use mpv_sys::mpv_error_MPV_ERROR_OPTION_FORMAT as OptionFormat;
    pub use mpv_sys::mpv_error_MPV_ERROR_OPTION_NOT_FOUND as OptionNotFound;
    pub use mpv_sys::mpv_error_MPV_ERROR_PROPERTY_ERROR as PropertyError;
    pub use mpv_sys::mpv_error_MPV_ERROR_PROPERTY_FORMAT as PropertyFormat;
    pub use mpv_sys::mpv_error_MPV_ERROR_PROPERTY_NOT_FOUND as PropertyNotFound;
    pub use mpv_sys::mpv_error_MPV_ERROR_PROPERTY_UNAVAILABLE as PropertyUnavailable;
    pub use mpv_sys::mpv_error_MPV_ERROR_SUCCESS as Success;
    pub use mpv_sys::mpv_error_MPV_ERROR_UNINITIALIZED as Uninitialized;
    pub use mpv_sys::mpv_error_MPV_ERROR_UNKNOWN_FORMAT as UnknownFormat;
    pub use mpv_sys::mpv_error_MPV_ERROR_UNSUPPORTED as Unsupported;
    pub use mpv_sys::mpv_error_MPV_ERROR_VO_INIT_FAILED as VoInitFailed;
}

/// Log verbosity level.
pub use mpv_sys::mpv_log_level as LogLevel;
pub mod mpv_log_level {
    #![allow(missing_docs)]
    pub use mpv_sys::mpv_log_level_MPV_LOG_LEVEL_DEBUG as Debug;
    pub use mpv_sys::mpv_log_level_MPV_LOG_LEVEL_ERROR as Error;
    pub use mpv_sys::mpv_log_level_MPV_LOG_LEVEL_FATAL as Fatal;
    pub use mpv_sys::mpv_log_level_MPV_LOG_LEVEL_INFO as Info;
    pub use mpv_sys::mpv_log_level_MPV_LOG_LEVEL_NONE as None;
    pub use mpv_sys::mpv_log_level_MPV_LOG_LEVEL_TRACE as Trace;
    pub use mpv_sys::mpv_log_level_MPV_LOG_LEVEL_V as V;
    pub use mpv_sys::mpv_log_level_MPV_LOG_LEVEL_WARN as Warn;
}

/// The reason a file stopped.
pub use mpv_sys::mpv_end_file_reason as EndFileReason;
pub mod mpv_end_file_reason {
    #![allow(missing_docs)]
    pub use mpv_sys::mpv_end_file_reason_MPV_END_FILE_REASON_EOF as Eof;
    pub use mpv_sys::mpv_end_file_reason_MPV_END_FILE_REASON_ERROR as Error;
    pub use mpv_sys::mpv_end_file_reason_MPV_END_FILE_REASON_QUIT as Quit;
    pub use mpv_sys::mpv_end_file_reason_MPV_END_FILE_REASON_REDIRECT as Redirect;
    pub use mpv_sys::mpv_end_file_reason_MPV_END_FILE_REASON_STOP as Stop;
}
