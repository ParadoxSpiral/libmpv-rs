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

#![allow(dead_code, unused_imports, unused_extern_crates)]

#[cfg(all(feature = "build_libmpv", not(windows)))]
extern crate git2 as git;
#[cfg(all(feature = "build_libmpv", windows))]
extern crate reqwest;

use std::env;
use std::io::{Error, Read, Seek, SeekFrom, Write};
use std::fs;
use std::fs::{File, OpenOptions};
use std::path::Path;
use std::process::Command;

const MPV_COMMIT: &'static str = "f60826c3a14ba3b49077f17e5364b7347f9b468a";
const WIN_MPV_ARCHIVE_SIZE: usize = 221629314;
const WIN_MPV_ARCHIVE_URL: &'static str = "https://mpv.srsfckn.biz/mpv-dev-20171225.7z";

#[cfg(all(feature = "events_simple", feature = "events_complex"))]
compile_error!(
    "Using events_simple and events_complex at the same time is forbidden, because it \
     will more likely than not result in a lot of hard to debug issues. This will later be \
     relaxed to a per mpv instance const generic flag."
);

#[cfg(not(feature = "build_libmpv"))]
fn main() {}

#[cfg(all(feature = "build_libmpv", target_os = "windows"))]
fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();

    let path = format!("{}/libmpv.7z", out_dir);
    let archive = OpenOptions::new().read(true).write(true).open(&path);

    let legacy = archive
        .as_ref()
        .map(|v| {
            if v.metadata().unwrap().len() != WIN_MPV_ARCHIVE_SIZE as u64 {
                Ok(())
            } else {
                // The returned error does not matter
                Err(())
            }
        })
        .is_ok();

    if archive.is_err() || legacy {
        let mut buf = Vec::with_capacity(WIN_MPV_ARCHIVE_SIZE);
        reqwest::get(WIN_MPV_ARCHIVE_URL)
            .unwrap()
            .read_to_end(&mut buf)
            .unwrap();

        if legacy {
            archive.as_ref().unwrap().set_len(0).unwrap();
            archive.unwrap().write_all(&buf).unwrap();
        } else {
            File::create(&path)
                .expect("failed to create file")
                .write_all(&buf)
                .unwrap();
        }

        Command::new("7z")
            .arg("x")
            .arg(&format!("-o{}", out_dir))
            .arg(path)
            .spawn()
            .expect("7z execution failed")
            .wait()
            .expect("7z execution failed");
    }

    if env::var("CARGO_CFG_TARGET_POINTER_WIDTH").unwrap() == "64" {
        println!("cargo:rustc-link-search={}/64/", out_dir);
    } else {
        println!("cargo:rustc-link-search={}/32/", out_dir);
    }
}

#[cfg(all(feature = "build_libmpv", not(target_os = "windows")))]
fn main() {
    // Assume unix like with sh
    use git::{ObjectType, Oid, Repository, ResetType};

    let out_dir = env::var("OUT_DIR").unwrap();

    // `target` (in cfg) doesn't really mean target. It means target(host) of build script,
    // which is a bit confusing because it means the actual `--target` everywhere else.
    #[cfg(target_pointer_width = "64")]
    {
        if env::var("CARGO_CFG_TARGET_POINTER_WIDTH").unwrap() == "32" {
            panic!("Cross-compiling to different arch not yet supported");
        }
    }
    #[cfg(target_pointer_width = "32")]
    {
        if env::var("CARGO_CFG_TARGET_POINTER_WIDTH").unwrap() == "64" {
            panic!("Cross-compiling to different arch not yet supported");
        }
    }

    let url = "https://github.com/mpv-player/mpv-build";
    let path = format!("{}/mpv-build", out_dir);
    let num_threads = env::var("NUM_JOBS").unwrap();
    let needs_rebuild;

    if !Path::new(&path).exists() {
        needs_rebuild = true;
        Repository::clone(url, &path).expect("failed to clone mpv-build");

        Command::new("sh")
            .arg("-c")
            .arg(&format!("cd {} && {0}/update", path))
            .spawn()
            .expect("mpv-build update failed")
            .wait()
            .expect("mpv-build update failed");

        let mpv_repo = Repository::open(&format!("{}/mpv/", path)).unwrap();
        mpv_repo
            .reset(
                &mpv_repo
                    .find_object(Oid::from_str(MPV_COMMIT).unwrap(), Some(ObjectType::Commit))
                    .unwrap(),
                ResetType::Soft,
                None,
            )
            .unwrap();
    } else {
        let mpv_repo = Repository::open(&format!("{}/mpv/", path)).unwrap();

        // If repo is older version cloned by older build script, update
        if mpv_repo
            .find_object(Oid::from_str(MPV_COMMIT).unwrap(), Some(ObjectType::Commit))
            .is_err()
        {
            needs_rebuild = true;
            Command::new("sh")
                .arg("-c")
                .arg(&format!("cd {} && {0}/update", path))
                .spawn()
                .expect("mpv-build update failed")
                .wait()
                .expect("mpv-build update failed");
        } else {
            needs_rebuild = false;
        }

        mpv_repo
            .reset(
                &mpv_repo
                    .find_object(Oid::from_str(MPV_COMMIT).unwrap(), Some(ObjectType::Commit))
                    .unwrap(),
                ResetType::Hard,
                None,
            )
            .unwrap();
    }

    // The mpv build script interprets the TARGET env var, which is set by cargo to e.g.
    // x86_64-unknown-linux-gnu, thus the script can't find the compiler.
    // TODO: When Cross-compiling to different archs is implemented, this has to be handled.
    env::remove_var("TARGET");

    let cmd = format!(
        "cd {} && echo \"--enable-libmpv-shared\" > {0}/mpv_options \
         && {0}/build -j{}",
        path, num_threads
    );

    if needs_rebuild {
        Command::new("sh")
            .arg("-c")
            .arg(&cmd)
            .spawn()
            .expect("mpv-build build failed")
            .wait()
            .expect("mpv-build build failed");
    }

    println!("cargo:rustc-link-search={}/mpv/build/", path);
}
