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

#[cfg(all(not(test), not(feature = "events_simple")))]
compile_error!("The feature `events_simple needs to be enabled for this example`");

#[cfg(feature = "events_simple")]
fn main() {
    use mpv::events::simple::*;
    use mpv::*;

    use std::env;
    use std::thread;
    use std::time::Duration;

    let path = env::args()
        .nth(1)
        .unwrap_or_else(|| String::from("https://www.youtube.com/watch?v=DLzxrzFCyOs"));

    // Create an `Mpv` and set some properties.
    let mpv = Mpv::new().unwrap();
    mpv.disable_deprecated_events().unwrap();
    mpv.set_property("cache-initial", 10).unwrap();
    mpv.set_property("volume", 15).unwrap();
    mpv.set_property("vo", "null").unwrap();

    crossbeam::scope(|scope| {
        scope.spawn(|| {
            mpv.playlist_load_files(&[(&path, FileState::AppendPlay, None)])
                .unwrap();

            thread::sleep(Duration::from_secs(3));

            mpv.set_property("volume", 25).unwrap();

            thread::sleep(Duration::from_secs(5));

            // Trigger `Event::EndFile`.
            mpv.playlist_next_force().unwrap();
        });
        scope.spawn(|| loop {
            let ev = unsafe { mpv.wait_event(600.) };
            if let Some(Ok(Event::EndFile(r))) = ev {
                println!("Exiting! Reason: {:?}", r);
                break;
            } else if let Some(Ok(e)) = ev {
                println!("Event triggered: {:?}", e);
            } else if let Some(Err(e)) = ev {
                println!("Event errored: {:?}", e);
            }
        });
    });
}
