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

extern crate crossbeam;

use mpv::*;
use mpv::events::*;
use mpv::utils::*;

use std::process;
use std::path::Path;
use std::time::Duration;
use std::thread;

pub fn exec() {
    // Create an `UninitializedParent` (with events enabled) to set some options.
    let mpv = UninitializedParent::<(), ()>::new(true).unwrap();
    mpv.set_option(Property::new("cache-initial", Data::new(1))).unwrap();
    mpv.set_option(Property::new("volume", Data::new(10))).unwrap();
    mpv.set_option(Property::new("vo", Data::new("null"))).unwrap();
    mpv.set_option(Property::new("ytdl", Data::new(true))).unwrap();
    // Consume the `UninitializedParent` and replace it by a `Parent`.
    let mpv = mpv.init().unwrap();

    // Create a crossbeam scope for convenience to use mpv in multiple threads.
    crossbeam::scope(|scope| {
        // Spin up 3 threads that observe different sets of `Event`s.
        scope.spawn(|| {
            let iter = mpv.observe_all(&[Event::FileLoaded,
                                         Event::StartFile,
                                         Event::Seek,
                                         Event::PlaybackRestart,
                                         Event::EndFile(None)])
                          .unwrap();

            for vec in iter {
                // If any `Event` was an `Endfile`, . . .
                if let Some(&Ok(Event::EndFile(ref v))) = vec.iter().find(|e| {
                    if e.is_ok() {
                        if let Event::EndFile(_) = *e.as_ref().unwrap() {
                            return true;
                        }
                    }
                    false
                }) {
                    // . . . print the `EndFile` reason and exit, . . .
                    println!("File ended! Reason: {:?}", v);
                    thread::sleep(Duration::from_secs(1));
                    process::exit(0);
                } else {
                    // . . . otherwise print all `Event`s.
                    println!("playback_events: {:?}", vec);
                };
            }
        });
        scope.spawn(|| {
            // Here the value of the `Property` is irrelevant: only the name is used.
            let iter = mpv.observe_all(&[Event::PropertyChange(Property::new("volume",
                                                                             Data::new(0))),
                                         Event::PropertyChange(Property::new("pause",
                                                                             Data::new(false)))])
                          .unwrap();

            for vec in iter {
                println!("prop_events: {:?}", vec);
            }
        });
        scope.spawn(|| {
            let iter = mpv.observe_all(&[Event::LogMessage(LogMessage::new(LogLevel::Info))])
                          .unwrap();

            for vec in iter {
                println!("log_events: {:#?}", vec);
            }
        });

        // Add a file to play, ytdl was set to true for this.
        mpv.playlist(&PlaylistOp::Loadfiles(&[File::new(Path::new("https://www.youtube.\
                                                                   com/watch?v=DLzxrzFCyOs"),
                                                        FileState::AppendPlay,
                                                        None)]))
           .unwrap();

        thread::sleep(Duration::from_secs(3));

        mpv.set_property(Property::new("volume", Data::new(25))).unwrap();

        thread::sleep(Duration::from_secs(30));

        // Trigger `Event::EndFile` observed above to quit.
        mpv.playlist(&PlaylistOp::NextForce).unwrap();
    });
}
