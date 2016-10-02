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
#![feature(pub_restricted, const_fn, stmt_expr_attributes, untagged_unions)]

//! This crate provides abstractions for (libmpv)[https://github.com/mpv-player/mpv/tree/master/libmpv] of the (mpv media player)[https://github.com/mpv-player/mpv].
//!
//! Libmpv requires `LC_NUMERIC` to be `C`. This is set once when the first parent is created.
//! Do not change this during the usage of this crate.
//!
//! # Examples
//!
//! See the 'examples' directory in the crate root.

extern crate libc;
extern crate parking_lot;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate enum_primitive;

/// Contains bindings to libmpv functions and data structures.
pub mod raw;
pub use raw::MpvLogLevel as LogLevel;
pub use raw::MpvError;
mod wrapper;
pub use wrapper::*;
#[cfg(test)]
mod tests;

#[allow(missing_docs)]
pub const MPV_CLIENT_API_MAJOR: u32 = 1;
#[allow(missing_docs)]
pub const MPV_CLIENT_API_MINOR: u32 = 23;
#[allow(missing_docs)]
pub const MPV_CLIENT_API_VERSION: u32 = mpv_make_version(MPV_CLIENT_API_MAJOR,
                                                         MPV_CLIENT_API_MINOR);
const fn mpv_make_version(major: u32, minor: u32) -> u32 {
    major << 16 | minor
}

#[all(feature="embed_libmpv", windows)]
#[cfg_attr(all(feature="embed_libmpv", windows), allow(missing_docs))]
// env var set in build script
pub const LIBMPV_DLL: &'static str = include_str!(env!("LIBMPV_LOCATION"));

#[all(feature="embed_libmpv", unix)]
#[cfg_attr(all(feature="embed_libmpv", unix), allow(missing_docs))]
pub const LIBMPV_A: &'static str = include_str!(env!("LIBMPV_LOCATION"));

#[cfg(feature="embed_libmpv")]
/// Note that the file has to be in a format the platform will recognize,
/// e.g. `mpv-1.dll` on windows and `libmpv.a` on unix.
pub fn write_libmpv_to_file(path: ::std::path::Path) -> Result<(), ::std::io::Error> {
	use std::io::prelude::*;

	let file = try!(File::create(path));
	#[windows] {
		try!(file.write_all(LIBMPV_DLL));
	}
	#[unix] {
		try!(file.write_all(LIBMPV_A));
	}
	Ok(())
}
