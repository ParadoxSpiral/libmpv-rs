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

use std::time::Duration;
use std::thread;

#[test]
fn version() {
    assert_eq!(super::MPV_CLIENT_API_VERSION,
               unsafe { raw::mpv_client_api_version() });
}

#[test]
fn properties() {
    let mpv = Mpv::new().unwrap();
    mpv.set_property("cache-initial", 1).unwrap();
    mpv.set_property("volume", 0).unwrap();
    mpv.set_property("vo", "null").unwrap();
    mpv.set_property("ytdl", true).unwrap();
    mpv.set_property("sub-gauss", 0.6).unwrap();

    assert_eq!(0i64, mpv.get_property("volume").unwrap());
    let vo: MpvStr = mpv.get_property("vo").unwrap();
    assert_eq!("null", &*vo);
    assert_eq!(true, mpv.get_property("ytdl").unwrap());
    let subg: f64 = mpv.get_property("sub-gauss").unwrap();
    assert_eq!(0.6,
               f64::round(subg * f64::powi(10.0, 4)) / f64::powi(10.0, 4));

    mpv.playlist_load_files(&[("https://www.youtube.com/watch?v=DLzxrzFCyOs",
                                FileState::AppendPlay,
                                None)])
        .unwrap();

    thread::sleep(Duration::from_millis(250));

    let title: MpvStr = mpv.get_property("media-title").unwrap();
    assert!("Rick Astley - Never Gonna Give You Up [HQ]" == &*title ||
            "watch?v=DLzxrzFCyOs" == &*title);
}

#[cfg(feature="events_simple")]
#[cfg_attr(feature="events_simple", test)]
fn events_simple() {
    use events::events_simple::{Event, PropertyData};

    let mpv = Mpv::new().unwrap();
    mpv.disable_deprecated_events().unwrap();
    mpv.observe_property("volume", Format::Int64, 0).unwrap();
    mpv.observe_property("media-title", Format::String, 1)
        .unwrap();
    mpv.observe_property("sub-gauss", Format::Double, 2)
        .unwrap();
    assert_eq!(Event::Idle, unsafe { mpv.wait_event(3.) }.unwrap().unwrap());

    mpv.set_property("cache-initial", 1).unwrap();
    mpv.set_property("volume", 0).unwrap();
    mpv.set_property("vo", "null").unwrap();
    assert_eq!(Event::PropertyChange {
                   name: "volume",
                   change: PropertyData::Int64(0),
                   reply_userdata: 0,
               },
               unsafe { mpv.wait_event(0.) }.unwrap().unwrap());
    assert_eq!(Event::PropertyChange {
                   name: "sub-gauss",
                   change: PropertyData::Double(0.),
                   reply_userdata: 2,
               },
               unsafe { mpv.wait_event(0.) }.unwrap().unwrap());

    mpv.playlist_load_files(&[("https://www.youtube.com/watch?v=DLzxrzFCyOs",
                                FileState::AppendPlay,
                                None)])
        .unwrap();
    assert_eq!(Event::StartFile,
               unsafe { mpv.wait_event(7.) }.unwrap().unwrap());
    assert_eq!(Event::PropertyChange {
                   name: "media-title",
                   change: PropertyData::Str("watch?v=DLzxrzFCyOs"),
                   reply_userdata: 1,
               },
               unsafe { mpv.wait_event(5.) }.unwrap().unwrap());
    assert_eq!(Err(ErrorKind::Native(::raw::mpv_error::MPV_ERROR_UNKNOWN_FORMAT).into()),
               unsafe { mpv.wait_event(5.) }.unwrap());
    assert_eq!(Event::Idle, unsafe { mpv.wait_event(0.) }.unwrap().unwrap());

    mpv.set_property("ytdl", true).unwrap();
    mpv.playlist_load_files(&[("https://www.youtube.com/watch?v=DLzxrzFCyOs",
                                FileState::AppendPlay,
                                None)])
        .unwrap();
    assert_eq!(Event::StartFile,
               unsafe { mpv.wait_event(10.) }.unwrap().unwrap());
    assert_eq!(Event::AudioReconfig,
               unsafe { mpv.wait_event(5.) }.unwrap().unwrap());
    assert_eq!(Event::FileLoaded,
               unsafe { mpv.wait_event(7.) }.unwrap().unwrap());
    assert_eq!(Event::PropertyChange {
                   name: "media-title",
                   change: PropertyData::Str("Rick Astley - Never Gonna Give You Up [HQ]"),
                   reply_userdata: 1,
               },
               unsafe { mpv.wait_event(5.) }.unwrap().unwrap());
    assert_eq!(Event::AudioReconfig,
               unsafe { mpv.wait_event(5.) }.unwrap().unwrap());
    assert_eq!(Event::AudioReconfig,
               unsafe { mpv.wait_event(5.) }.unwrap().unwrap());
    assert_eq!(Event::VideoReconfig,
               unsafe { mpv.wait_event(5.) }.unwrap().unwrap());
    assert_eq!(Event::VideoReconfig,
               unsafe { mpv.wait_event(5.) }.unwrap().unwrap());
    assert_eq!(Event::PlaybackRestart,
               unsafe { mpv.wait_event(5.) }.unwrap().unwrap());

    assert_eq!(None, unsafe { mpv.wait_event(0.) });

    // TODO: Expand to all Event variants
}
