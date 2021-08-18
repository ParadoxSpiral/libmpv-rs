// Copyright (C) 2021  anlumo
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
use glium::{
    glutin::{
        dpi::LogicalSize,
        event::{Event, WindowEvent},
        event_loop::{ControlFlow, EventLoop},
        window::WindowBuilder,
        ContextBuilder,
    },
    Display,
};
use libmpv::{
    render::{OpenGLInitParams, RenderContext, RenderParam, RenderParamApiType},
    FileState, Mpv,
};
use std::{env, ffi::c_void};

fn get_proc_address(display: &Display, name: &str) -> *mut c_void {
    display.gl_window().context().get_proc_address(name) as *mut c_void
}

const VIDEO_URL: &str = "https://www.youtube.com/watch?v=DLzxrzFCyOs";

#[derive(Debug)]
enum UserEvent {
    MpvEventAvailable,
    RedrawRequested,
}

fn main() {
    let path = env::args()
        .nth(1)
        .unwrap_or_else(|| String::from(VIDEO_URL));

    let events_loop = EventLoop::<UserEvent>::with_user_event();
    let wb = WindowBuilder::new()
        .with_inner_size(LogicalSize::new(1024.0, 768.0))
        .with_title("libmpv-rs OpenGL Example");
    let cb = ContextBuilder::new();
    let display = Display::new(wb, cb, &events_loop).unwrap();

    let mut mpv = Mpv::new().expect("Error while creating MPV");
    let mut render_context = RenderContext::new(
        unsafe { mpv.ctx.as_mut() },
        vec![
            RenderParam::ApiType(RenderParamApiType::OpenGl),
            RenderParam::InitParams(OpenGLInitParams {
                get_proc_address,
                ctx: display.clone(),
            }),
        ],
    )
    .expect("Failed creating render context");
    mpv.event_context_mut().disable_deprecated_events().unwrap();
    let event_proxy = events_loop.create_proxy();
    render_context.set_update_callback(move || {
        event_proxy.send_event(UserEvent::RedrawRequested).unwrap();
    });
    let event_proxy = events_loop.create_proxy();
    mpv.event_context_mut().set_wakeup_callback(move || {
        event_proxy
            .send_event(UserEvent::MpvEventAvailable)
            .unwrap();
    });
    let mut render_context = Some(render_context);
    mpv.playlist_load_files(&[(&path, FileState::AppendPlay, None)])
        .unwrap();
    events_loop.run(move |event, _target, control_flow| {
        match event {
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                ..
            } => {
                *control_flow = ControlFlow::Exit;
            }
            Event::UserEvent(UserEvent::RedrawRequested) => {
                display.gl_window().window().request_redraw();
            }
            Event::UserEvent(UserEvent::MpvEventAvailable) => loop {
                match mpv.event_context_mut().wait_event(0.0) {
                    Some(Ok(libmpv::events::Event::EndFile(_))) => {
                        *control_flow = ControlFlow::Exit;
                        break;
                    }
                    Some(Ok(mpv_event)) => {
                        eprintln!("MPV event: {:?}", mpv_event);
                    }
                    Some(Err(err)) => {
                        eprintln!("MPV Error: {}", err);
                        *control_flow = ControlFlow::Exit;
                        break;
                    }
                    None => {
                        *control_flow = ControlFlow::Wait;
                        break;
                    }
                }
            },
            Event::RedrawRequested(_) => {
                if let Some(render_context) = &render_context {
                    let (width, height) = display.get_framebuffer_dimensions();
                    render_context
                        .render::<Display>(0, width as _, height as _, true)
                        .expect("Failed to draw on glutin window");
                    display.swap_buffers().unwrap();
                }
                *control_flow = ControlFlow::Wait;
            }
            Event::LoopDestroyed => {
                render_context.take(); // It's important to destroy the render context before the mpv player!
            }
            _ => {
                *control_flow = ControlFlow::Wait;
            }
        }
    });
}
