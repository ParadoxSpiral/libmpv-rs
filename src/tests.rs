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

#![allow(unused)]

use super::*;

use std::time::Duration;
use std::thread;

#[test]
fn properties() {
    let mpv = Mpv::new().unwrap();
    mpv.set_property("cache-initial", 1).unwrap();
    mpv.set_property("volume", 0).unwrap();
    mpv.set_property("vo", "null").unwrap();
    mpv.set_property("ytdl-format", "best[width<240]").unwrap();
    mpv.set_property("sub-gauss", 0.6).unwrap();

    assert_eq!(0i64, mpv.get_property("volume").unwrap());
    let vo: MpvStr = mpv.get_property("vo").unwrap();
    assert_eq!("null", &*vo);
    assert_eq!(true, mpv.get_property("ytdl").unwrap());
    let subg: f64 = mpv.get_property("sub-gauss").unwrap();
    assert_eq!(
        0.6,
        f64::round(subg * f64::powi(10.0, 4)) / f64::powi(10.0, 4)
    );

    mpv.playlist_load_files(&[
        (
            "https://www.youtube.com/watch?v=DLzxrzFCyOs",
            FileState::AppendPlay,
            None,
        ),
    ]).unwrap();

    thread::sleep(Duration::from_millis(250));

    let title: MpvStr = mpv.get_property("media-title").unwrap();
    assert!(
        "Rick Astley - Never Gonna Give You Up [HQ]" == &*title || "watch?v=DLzxrzFCyOs" == &*title
    );
}

// Used to approximate correctness of non-deterministic event order
macro_rules! assert_eq_any {
    ($left:expr, $( $right:expr ),+) => (
        {
            let val = $left;
            if $( val != $right )&&+ {
                panic!("assertion failed: `(left == any [right])` (left: {:?}, right: {:?})",
                       val,
                       [$( $right ),+]);
            }
        }
    )
}

#[cfg(feature = "events_simple")]
#[cfg_attr(feature = "events_simple", test)]
fn events_simple() {
    // TODO: Expand to all Event variants
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
    assert_eq!(
        Event::PropertyChange {
            name: "volume",
            change: PropertyData::Int64(0),
            reply_userdata: 0,
        },
        unsafe { mpv.wait_event(3.) }.unwrap().unwrap()
    );
    assert_eq!(
        Event::PropertyChange {
            name: "sub-gauss",
            change: PropertyData::Double(0.),
            reply_userdata: 2,
        },
        unsafe { mpv.wait_event(3.) }.unwrap().unwrap()
    );

    mpv.playlist_load_files(&[
        (
            "https://www.youtube.com/watch?v=DLzxrzFCyOs",
            FileState::AppendPlay,
            None,
        ),
    ]).unwrap();
    assert_eq!(
        Event::StartFile,
        unsafe { mpv.wait_event(10.) }.unwrap().unwrap()
    );
    assert_eq!(
        Event::PropertyChange {
            name: "media-title",
            change: PropertyData::Str("watch?v=DLzxrzFCyOs"),
            reply_userdata: 1,
        },
        unsafe { mpv.wait_event(10.) }.unwrap().unwrap()
    );
    assert_eq!(
        Err(Error::Raw(mpv_error::UnknownFormat)),
        unsafe { mpv.wait_event(20.) }.unwrap()
    );
    assert_eq!(Event::Idle, unsafe { mpv.wait_event(4.) }.unwrap().unwrap());

    mpv.set_property("ytdl", true).unwrap();
    mpv.set_property("ytdl-format", "best[width<240]").unwrap();
    mpv.playlist_load_files(&[
        (
            "https://www.youtube.com/watch?v=DLzxrzFCyOs",
            FileState::AppendPlay,
            None,
        ),
    ]).unwrap();
    // The order of events is unfortunately non-deterministic.
    for _ in 0..5 {
        // A possible order is:
        //      StartFile -> AudioReconfig -> FileLoaded -> AudioReconfig -> PropertyChange
        assert_eq_any!(
            unsafe { mpv.wait_event(10.) }.unwrap().unwrap(),
            Event::StartFile,
            Event::AudioReconfig,
            Event::FileLoaded,
            // Either both, or only the second title Event will trigger because of coalescence
            Event::PropertyChange {
                name: "media-title",
                change: PropertyData::Str("watch?v=DLzxrzFCyOs"),
                reply_userdata: 1,
            },
            Event::PropertyChange {
                name: "media-title",
                change: PropertyData::Str("Rick Astley - Never Gonna Give You Up [HQ]"),
                reply_userdata: 1,
            }
        );
    }
    assert_eq!(
        Event::AudioReconfig,
        unsafe { mpv.wait_event(10.) }.unwrap().unwrap()
    );
    assert_eq!(
        Event::VideoReconfig,
        unsafe { mpv.wait_event(10.) }.unwrap().unwrap()
    );
    assert_eq!(
        Event::VideoReconfig,
        unsafe { mpv.wait_event(10.) }.unwrap().unwrap()
    );
    assert_eq!(
        Event::PlaybackRestart,
        unsafe { mpv.wait_event(10.) }.unwrap().unwrap()
    );

    assert_eq!(None, unsafe { mpv.wait_event(0.) });
}
