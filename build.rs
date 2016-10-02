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

#![feature(stmt_expr_attributes)]
#![allow(unused_imports)]

extern crate hyper;
extern crate git2 as git;

use git::Repository;

use std::env;
use std::io::{Read, Write};
use std::fs::File;
use std::path::Path;
use std::process::Command;

fn main() {
	#[cfg(feature="build_libmpv")] {
		let out_dir = env::var("OUT_DIR").unwrap();
		let target = env::var("TARGET").unwrap();

		if target.contains("windows") {
			let path = format!("{}/libmpv.7z", out_dir);
			let archive = Path::new(&path);
			
			if !archive.exists() {
				let mut buf = Vec::with_capacity(128*100);
				hyper::Client::new().get("https://mpv.srsfckn.biz/mpv-dev-20160826.7z")
									.send().expect("retrieving libmpv failed")
									.read_to_end(&mut buf).unwrap();

				File::create(archive)
					.expect("failed to open file!")
					.write_all(&buf)
					.unwrap();
			}

			Command::new("7z").arg("x").arg(&format!("-o{}", out_dir))
							  .arg(archive).output().expect("7z execution failed");

			#[cfg(target_pointer_width = "32")] {
				println!("cargo:rustc-link-search=native={}/32/", out_dir);
			}
			#[cfg(target_pointer_width = "64")] {
				println!("cargo:rustc-link-search=native={}/64/", out_dir);
			}
		} else {
		    // Assume unix like

		    let url = "https://github.com/mpv-player/mpv-build";
		    let path = format!("{}/libmpv", out_dir);
			let num_threads = env::var("NUM_JOBS").unwrap();

			if Path::new(&path).exists() {
			    Command::new("sh")
							  .arg("-c")
							  .arg(&format!("{}/build -j{}", path, num_threads))
							  .output().expect("libmpv build failed");
			} else {
				Repository::clone(url, &path).expect("failed to mpv-buikd");
				Command::new("sh")
							  .arg("-c")
							  .arg(&format!("cd {} && ./update && echo --enable-libmpv-shared > \
							   				mpv_options && ./build -j{}",
							  				path, config, num_threads))
							  .output().expect("libmpv build failed");
			}
			println!("cargo:rustc-link-search=native={}/mpv/build/", path);
		}
		
	    println!("cargo:rustc-link-lib=mpv");
	}
}
