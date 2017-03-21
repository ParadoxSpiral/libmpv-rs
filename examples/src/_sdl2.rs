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

use crossbeam;
use mpv::*;
use mpv::opengl_cb::*;
use parking_lot::Condvar;
use sdl2;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;

use std::env;
use std::panic::AssertUnwindSafe;
use std::sync::{Arc, Mutex};
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::{Duration, Instant};
use std::thread;

static RUNNING: AtomicBool = AtomicBool::new(true);

pub fn exec() {
    let path = env::args().nth(1).expect("Expected path to media as argument, found nil.");

    // Check for driver availability
    let driver_index = if let Some(i) = sdl2::render::drivers().position(|e| e.name == "opengl") {
        i
    } else {
        panic!("OpenGl driver not found!")
    };

    crossbeam::scope(|scope| {
        // Create SDL state
        let sdl_context = sdl2::init().unwrap();
        let video_subsystem = sdl_context.video().unwrap();
        let window = video_subsystem.window("mpv-rs sdl2 example", 1280, 720)
            .resizable()
            .position_centered()
            .opengl()
            .build()
            .unwrap();
        let renderer = window.renderer()
            .index(driver_index as _)
            .build()
            .expect("Failed to create renderer with given parameters");

        // Create mpv state
        let mpv = Parent::with_options(false,
                                       &[("vo", "opengl-cb".into()),
                                         ("cache-initial", 1.into()),
                                         ("volume", 30.into()),
                                         ("ytdl", true.into())])
                .unwrap();
        let mut mpv_ogl = mpv.create_opengl_context(|name| {
            (*AssertUnwindSafe(|name| -> *const () {
                video_subsystem.gl_get_proc_address(name)
            }))(name)
        })
                             .unwrap();
        let notifier = Condvar::new();
        mpv_ogl.set_update_callback(AssertUnwindSafe(&notifier), gl_update_callback);

        // Setup input event polling at 20hz
        scope.spawn(move || {
            let mut event_pump = sdl_context.event_pump().unwrap();
            let sync = Duration::from_millis(1000 / 20);
            'main: loop {
                let initial = Instant::now();
                for event in event_pump.poll_iter() {
                    match event {
                        Event::Quit { .. } |
                        Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                            RUNNING.store(false, Ordering::Release);
                            notifier.notify_one();
                            break 'main;
                        }
                        Event::KeyDown { keycode: Some(Keycode::Space), repeat: false, .. } => {
                            if let Data::Flag(true) = mpv.get_property("pause", Format::Flag)
                                   .unwrap() {
                                mpv.unpause().unwrap();
                            } else {
                                mpv.pause().unwrap();
                            }
                        }
                        Event::KeyDown { keycode: Some(Keycode::Right), repeat: false, .. } => {
                            mpv.seek_forward(&Duration::from_secs(10)).unwrap();
                        }
                        _ => {}
                    }
                }
                let diff = Instant::now() - initial;
                if diff < sync {
                    thread::sleep(sync - diff);
                }
            }
        });

        // Setup drawing
        scope.spawn(move || {
            use parking_lot::Mutex;

            let wref = renderer.window().unwrap();
            wref.gl_set_context_to_current().unwrap();
            let mut guard = Mutex::new(false);
            loop {
                if RUNNING.load(Ordering::Acquire) {
                    notifier.wait(&mut guard.lock());
                    let (width, height) = wref.size();
                    unsafe {
                        mpv_ogl.draw(0, width as _, -(height as isize)).unwrap();
                    }
                    wref.gl_swap_window();
                }
            }
        });

        // Load specified file
        mpv.playlist_load_files(&[(&path, FileState::AppendPlay, None)]).unwrap();
    });
}

fn gl_update_callback(notifier: &AssertUnwindSafe<&Condvar>) {
    notifier.notify_one();
}
