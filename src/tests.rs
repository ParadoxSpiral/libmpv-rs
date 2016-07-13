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

#[test]
fn version() {
    assert_eq!(super::MPV_CLIENT_API_VERSION,
               unsafe { raw::mpv_client_api_version() });
}

#[test]
fn options_properties() {
    // TODO: Cover all `Data` variants.

    let mpv = Parent::new(false).unwrap();
    mpv.set_option(&mut Property::new("cache-initial", Data::new(1))).unwrap();
    mpv.set_option(&mut Property::new("volume", Data::new(0))).unwrap();
    mpv.set_option(&mut Property::new("no-video", Data::new(true))).unwrap();
    mpv.set_option(&mut Property::new("ytdl", Data::new(true))).unwrap();
    mpv.init().unwrap();

    mpv.playlist(&PlaylistOp::Loadfiles(&[File::new(::std::path::Path::new("https://www.youtube.\
                                                                   com/watch?v=DLzxrzFCyOs"),
                                                        FileState::AppendPlay,
                                                        None)]))
           .unwrap();
    ::std::thread::sleep(::std::time::Duration::from_secs(7));

    assert_eq!(Data::new(0),
               mpv.get_property("volume", &Format::Int64)
                  .unwrap());

    mpv.set_property(&mut Property::new("volume", Data::new(4))).unwrap();

    assert_eq!(Data::new(4),
               mpv.get_property("volume", &Format::Int64)
                  .unwrap());

    assert_eq!(Data::new("Rick Astley - Never Gonna Give You Up [HQ]".to_owned()),
               mpv.get_property("media-title", &Format::String).unwrap());
}
