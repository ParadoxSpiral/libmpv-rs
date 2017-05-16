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

extern crate bindgen;

use std::env;
use std::path::PathBuf;

fn main() {
    println!("cargo:rustc-link-lib=mpv");

    #[cfg(feature="nightly")]
    let bindings = bindgen::Builder::default()
        .header("src/client.h")
        .header("src/opengl_cb.h")
        .header("src/stream_cb.h")
        .hide_type("max_align_t")
        .generate_comments(false)
        .opaque_type("mpv_handle")
        .opaque_type("mpv_opengl_cb_context")
        .generate()
        .expect("Unable to generate bindings");
    #[cfg(not(feature="nightly"))]
    let bindings = bindgen::Builder::default()
        .no_unstable_rust()
        .header("src/client.h")
        .header("src/opengl_cb.h")
        .header("src/stream_cb.h")
        .hide_type("max_align_t")
        .generate_comments(false)
        .opaque_type("mpv_handle")
        .opaque_type("mpv_opengl_cb_context")
        .generate()
        .expect("Unable to generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
