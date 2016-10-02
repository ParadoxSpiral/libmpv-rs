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

use super::*;

use std::path::Path;
use std::time::Duration;
use std::thread;

#[test]
fn version() {
    assert_eq!(super::MPV_CLIENT_API_VERSION,
               unsafe { raw::mpv_client_api_version() });
}

#[test]
fn properties() {
    let mpv = Parent::<(), ()>::new(false).unwrap();
    mpv.set_property("cache-initial", 1).unwrap();
    mpv.set_property("volume", 0).unwrap();
    mpv.set_property("vo", "null").unwrap();
    mpv.set_property("ytdl", true).unwrap();

    mpv.playlist(&PlaylistOp::Loadfiles(&[File::new(Path::new("https://www.youtube.\
                                                                   com/watch?v=DLzxrzFCyOs"),
                                                        FileState::AppendPlay,
                                                        None)]))
           .unwrap();

    thread::sleep(Duration::from_millis(250));

    assert_eq!(Data::new(0),
               mpv.get_property("volume", &Format::Int64)
                  .unwrap());

    let title = mpv.get_property("media-title", &Format::String).unwrap();
    assert!(Data::new("Rick Astley - Never Gonna Give You Up [HQ]".to_owned()) == title ||
            Data::new("watch?v=DLzxrzFCyOs".to_owned()) == title);
}
