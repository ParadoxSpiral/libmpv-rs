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

extern crate mpv;
extern crate crossbeam;

use mpv::*;
use mpv::events::*;

use std::env;
use std::process;
use std::time::Duration;
use std::thread;

pub fn main() {
    let path = env::args()
        .nth(1)
        .expect("Expected path to media as argument, found nil.");

    // Create a `Parent` (with events enabled) and set some properties.
    let mpv = Parent::with_options(true,
                                   &[("cache-initial", 1.into()),
                                     ("volume", 10.into()),
                                     ("vo", "null".into()),
                                     ("ytdl", true.into())])
            .unwrap();

    // Create a crossbeam scope for convenience to use mpv in multiple threads.
    crossbeam::scope(|scope| {
        // Spin up 3 threads that observe different sets of `Event`s.
        scope.spawn(|| {
            let iter = mpv.observe_events(&[Event::FileLoaded,
                                            Event::StartFile,
                                            Event::Seek,
                                            Event::PlaybackRestart,
                                            Event::EndFile {
                                                reason: EndFileReason::Eof,
                                                error: None,
                                            }])
                .unwrap();

            for vec in iter {
                // If any `Event` was an `Endfile`, . . .
                if let Some(&Event::EndFile {
                                 reason: ref r,
                                 error: ref e,
                             }) =
                    vec.iter()
                        .find(|ev| if let Event::EndFile { .. } = **ev {
                                  true
                              } else {
                                  false
                              }) {
                    // . . . print the `EndFile` reason and exit, . . .
                    println!("File ended! Reason: {:?}; Error: {:?}", r, e);
                    thread::sleep(Duration::from_millis(300));
                    process::exit(0);
                } else {
                    // . . . otherwise print all `Event`s.
                    println!("playback: {:?}", vec);
                };
            }
        });
        scope.spawn(|| {
            // Here the value of the property is irrelevant: only the name is used.
            let iter = mpv.observe_events(&[Event::PropertyChange(("volume".into(), PropertyData::Int64(0))),
                                            Event::PropertyChange(("pause".into(), PropertyData::Flag(false)))])
                .unwrap();

            for vec in iter {
                println!("properties: {:?}", vec);
            }
        });
        scope.spawn(|| {
                        let iter = mpv.observe_events(&[Event::empty_logmessage(LogLevel::Info)])
                            .unwrap();

                        for vec in iter {
                            println!("log: {:#?}", vec);
                        }
                    });

        // Add a file to play, ytdl was set to true for this.
        mpv.playlist_load_files(&[(&path, FileState::AppendPlay, None)])
            .unwrap();

        thread::sleep(Duration::from_secs(3));

        mpv.set_property("volume", 25).unwrap();

        thread::sleep(Duration::from_secs(2));

        // Trigger `Event::EndFile` observed above to quit.
        mpv.playlist_next_force().unwrap();
    });
}
