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
#![allow(dead_code, unused_imports)]

extern crate hyper;
extern crate git2 as git;

use git::{ResetType, Repository, Oid, ObjectType};

use std::env;
use std::io::{Read, Write, Error, Seek, SeekFrom};
use std::fs;
use std::fs::{File, OpenOptions};
use std::path::Path;
use std::process::Command;

fn main() {
	#[cfg(feature="build_libmpv")] {
		let out_dir = env::var("OUT_DIR").unwrap();
		let target = env::var("TARGET").unwrap();

		if target.contains("windows") {
			let path = format!("{}/libmpv.7z", out_dir);
            let archive = File::open(&path);

            let dummy_err = Error::last_os_error();
            let legacy = archive.as_ref().and_then(|v| {
                            if v.metadata().unwrap().len() != 109631953 {
                                Ok(&dummy_err)
                            } else {
                                // The returned error does not matter
                                Err(&dummy_err)
                            }
                        }).is_ok();

			if archive.is_err() || legacy {
				let mut buf = Vec::with_capacity(109631953);

				hyper::Client::new().get("https://mpv.srsfckn.biz/mpv-dev-20161020.7z")
									.send().expect("retrieving libmpv failed")
									.read_to_end(&mut buf).unwrap();

                if legacy {
                    archive.as_ref().unwrap().set_len(0).unwrap();
                    archive.unwrap().write_all(&buf).unwrap();
                } else {
                    File::create(&path).expect("failed to create file").write_all(&buf).unwrap();
                }

                Command::new("7z").arg("x").arg(&format!("-o{}", out_dir)).arg(path)
                                  .env_clear().spawn().expect("7z execution failed")
                                  .wait().expect("7z execution failed");
			}

			if target.contains("x86_64") {
				println!("cargo:rustc-link-search=native={}/64/", out_dir);
			} else {
				println!("cargo:rustc-link-search=native={}/32/", out_dir);
			}
		} else {
		    // Assume unix like

		    // `target` doesn't really mean target. It means target(host) of build script, which is
		    // a bit confusing because it means the actual `--target` everywhere else.
		    #[cfg(target_pointer_width = "64")] {
		    	if (target.contains("x86") && ! target.contains("x86_64")) ||
		    	    target.contains("i686") {
		    		panic!("Cross-compiling to different arch not yet supported");
		    	}
		    }
		    #[cfg(target_pointer_width = "32")] {
		    	if target.contains("x86_64") {
		    		panic!("Cross-compiling to different arch not yet supported");
		    	}
		    }

		    let url = "https://github.com/mpv-player/mpv-build";
		    let path = format!("{}/mpv-build", out_dir);
			let num_threads = env::var("NUM_JOBS").unwrap();

			if !Path::new(&path).exists() {
                Repository::clone(url, &path).expect("failed to clone mpv-build");

                Command::new("sh")
                              .arg("-c")
                              .arg(&format!("cd {} && {0}/update", path))
                              .env_clear()
                              .spawn().expect("mpv-build update failed")
                              .wait().expect("mpv-build update failed");

                let mpv_repo = Repository::open(&format!("{}/mpv/", path)).unwrap();
                mpv_repo.reset(&mpv_repo.find_object(
                                 Oid::from_str("90b968a67a73473d615e2ee6135756573d0da6f2").unwrap(),
                                 Some(ObjectType::Commit)).unwrap(),
                               ResetType::Soft,
                               None).unwrap();
			} else {
                let mpv_repo = Repository::open(&format!("{}/mpv/", path)).unwrap();
                
                // If repo is older version cloned by older build script, update
                if mpv_repo.find_object(Oid::from_str("90b968a67a73473d615e2ee6135756573d0da6f2")
                                            .unwrap(),
                                        Some(ObjectType::Commit)).is_err()
                {
                    Command::new("sh")
                                  .arg("-c").arg(&format!("cd {} && {0}/update", path))
                                  .env_clear()
                                  .spawn().expect("mpv-build update failed")
                                  .wait().expect("mpv-build update failed");
                }

                Command::new("sh")
                        .arg("-c")
                        .arg(&format!("cd {} && {0}/clean", path))
                        .env_clear()
                        .spawn().expect("mpv-build clean failed")
                        .wait().expect("mpv-build clean failed");

                mpv_repo.reset(&mpv_repo.find_object(
                                    Oid::from_str("90b968a67a73473d615e2ee6135756573d0da6f2")
                                        .unwrap(),
                                    Some(ObjectType::Commit)).unwrap(),
                               ResetType::Soft,
                               None).unwrap();
            }

            panic!("{:?}", Command::new("sh")
                                .arg("-c")
                                .arg(&format!("cd {} && echo \"--enable-libmpv-shared\" > \
                                    {0}/mpv_options && {0}/build -j{}", path, num_threads))
                                .env_clear()
                                .spawn().expect("mpv-build build failed")
                                .wait().expect("mpv-build build failed"));

			println!("cargo:rustc-link-search=native={}/mpv/build/", path);
		}
		
	    println!("cargo:rustc-link-lib=mpv");
	}
}
