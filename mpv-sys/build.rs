// Copyright (C) 2016  ParadoxSpiral
//
// This file is part of mpv-sys.
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

#[cfg(feature = "bindgen")]
extern crate bindgen;

use std::{env, fs};
use std::path::PathBuf;

#[cfg(feature = "bindgen")]
fn generate_bindings() {
    let bindings = bindgen::Builder::default()
        .header("include/client.h")
        .header("include/opengl_cb.h")
        .header("include/stream_cb.h")
        .hide_type("max_align_t")
        .opaque_type("mpv_handle")
        .opaque_type("mpv_opengl_cb_context")
        // This needs to be disabled until we do static builds
        //.clang_arg("-DMPV_ENABLE_DEPRECATED=0")
        .generate()
        .expect("Unable to generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}

#[cfg(not(feature = "bindgen"))]
fn copy_pregenerated_bindings() {
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    let crate_path = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    fs::copy(crate_path.join("pregenerated_bindings.rs"), out_path.join("bindings.rs"))
        .expect("Couldn't find pregenerated bindings!");
}

fn link_mpv() {
    println!("cargo:rustc-link-lib=mpv");
}

fn main() {
    #[cfg(not(feature = "bindgen"))] {
        copy_pregenerated_bindings();
    }

    #[cfg(feature = "bindgen")] {
        generate_bindings();
    }

    link_mpv();
}
