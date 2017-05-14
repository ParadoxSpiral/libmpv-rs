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
extern crate sdl2;

use mpv::*;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;

use std::env;
use std::panic::AssertUnwindSafe;
use std::rc::Rc;
use std::time::Duration;

struct Draw {}

pub fn main() {
    let path = env::args()
        .nth(1)
        .expect("Expected path to media as argument, found nil.");

    // Check for driver availability
    let driver_index = if let Some(i) = sdl2::render::drivers().position(|e| e.name == "opengl") {
        i
    } else {
        panic!("OpenGl driver not found!")
    };

    // Create SDL state
    let sdl = sdl2::init().unwrap();
    let video_subsystem = Rc::new(sdl.video().unwrap());
    let vs = video_subsystem.clone();
    let window = vs.window("mpv-rs sdl2 example", 1280, 720)
        .resizable()
        .position_centered()
        .opengl()
        .build()
        .unwrap();
    let renderer = window
        .renderer()
        .index(driver_index as _)
        .build()
        .expect("Failed to create renderer with given parameters");
    let wref = renderer.window().unwrap();
    let mut event_pump = sdl.event_pump().unwrap();
    let events = sdl.event().unwrap();
    events.register_custom_event::<Draw>().unwrap();

    // Create mpv state
    let mpv = Parent::new().unwrap();
    mpv.set_property("vo", "opengl-cb").unwrap();
    mpv.set_property("cache-initial", 1).unwrap();
    mpv.set_property("volume", 25).unwrap();
    mpv.set_property("ytdl", true).unwrap();

    let vs = video_subsystem.clone();
    let mut mpv_ogl = mpv.create_opengl_context(move |name| vs.gl_get_proc_address(name))
        .unwrap();
    unsafe {
        mpv_ogl.set_update_callback(AssertUnwindSafe(events),
                                    |events| events.push_custom_event(Draw {}).unwrap())
    };

    // Load specified file
    mpv.playlist_load_files(&[(&path, FileState::AppendPlay, None)])
        .unwrap();

    // Setup event handling
    'main: loop {
        for event in event_pump.wait_iter() {
            if event.is_user_event() {
                // The only user event is `Draw`, we don't have to check.
                let (width, height) = wref.size();
                unsafe {
                    mpv_ogl.draw(0, width as _, -(height as isize)).unwrap();
                }
                wref.gl_swap_window();
                continue;
            }

            match event {
                Event::Quit { .. } |
                Event::KeyDown { keycode: Some(Keycode::Escape), .. } |
                Event::KeyDown { keycode: Some(Keycode::Q), .. } => {
                    break 'main;
                }
                Event::KeyDown {
                    keycode: Some(Keycode::Space),
                    repeat: false,
                    ..
                } => {
                    if mpv.get_property("pause").unwrap() {
                        mpv.unpause().unwrap();
                    } else {
                        mpv.pause().unwrap();
                    }
                }
                Event::KeyDown {
                    keycode: Some(Keycode::Right),
                    repeat: false,
                    ..
                } => {
                    mpv.seek_forward(&Duration::from_secs(10)).unwrap();
                }
                _ => {}
            }
        }
    }
}
