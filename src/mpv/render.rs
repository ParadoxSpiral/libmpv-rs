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

use libmpv_sys;
use std::convert::From;
use std::ffi::{c_void, CStr, CString};
use std::ptr;
use std::collections::HashMap;

pub struct RenderContext {
    ctx: *mut libmpv_sys::mpv_render_context,
    // This is our little dirty bag of raw pointers that need specific
    // deallocation. The rendercontext typically took ownership of these when
    // renderparams were passed.
    raw_ptrs: HashMap<*mut c_void, fn(*mut c_void)>
}

/// For initializing the mpv OpenGL state via RenderParam::OpenGLInitParams
pub struct OpenGLInitParams<GLContext> {
    /// This retrieves OpenGL function pointers, and will use them in subsequent
    /// operation.
    /// Usually, you can simply call the GL context APIs from this callback (e.g.
    /// glXGetProcAddressARB or wglGetProcAddress), but some APIs do not always
    /// return pointers for all standard functions (even if present); in this
    /// case you have to compensate by looking up these functions yourself when
    /// libmpv wants to resolve them through this callback.
    /// libmpv will not normally attempt to resolve GL functions on its own, nor
    /// does it link to GL libraries directly.
    pub get_proc_address: fn(ctx: &GLContext, name: &str) -> *mut c_void,

    /// Value passed as ctx parameter to get_proc_address().
    pub ctx: GLContext,
}

/// For RenderParam::FBO
pub struct FBO {
    pub fbo: i32,
    pub width: i32,
    pub height: i32,
}

#[repr(u8)]
pub enum RenderFrameInfo {
    Present = 1 << 0,
    Redraw = 1 << 1,
    Repeat = 1 << 2,
    BlockVSync = 1 << 3,
}

pub enum RenderParam<GLContext> {
    Invalid,
    ApiType(&'static str),
    InitParams(OpenGLInitParams<GLContext>),
    FBO(FBO),
    FlipY(bool),
    Depth(i32),
    ICCProfile(Vec<u8>),
    AmbientLight(i32),
    X11Display(*const c_void),
    WaylandDisplay(*const c_void),
    AdvancedControl(bool),
    NextFrameInfo(Box<RenderFrameInfo>),
    BlockForTargetTime(bool),
    SkipRendering(bool),
}

impl<C> From<&RenderParam<C>> for u32 {
    fn from(val: &RenderParam<C>) -> Self {
        match val {
            RenderParam::Invalid => 0,
            RenderParam::ApiType(_) => 1,
            RenderParam::InitParams(_) => 2,
            RenderParam::FBO(_) => 3,
            RenderParam::FlipY(_) => 4,
            RenderParam::Depth(_) => 5,
            RenderParam::ICCProfile(_) => 6,
            RenderParam::AmbientLight(_) => 7,
            RenderParam::X11Display(_) => 8,
            RenderParam::WaylandDisplay(_) => 9,
            RenderParam::AdvancedControl(_) => 10,
            RenderParam::NextFrameInfo(_) => 11,
            RenderParam::BlockForTargetTime(_) => 12,
            RenderParam::SkipRendering(_) => 13,
        }
    }
}

unsafe extern "C" fn gpa_wrapper<GLContext>(ctx: *mut c_void, name: *const i8) -> *mut c_void {
    if ctx == ptr::null_mut() {
        panic!("ctx for get_proc_address wrapper is NULL");
    }

    use std::mem::transmute;

    let params: *mut OpenGLInitParams<GLContext> = transmute(ctx);
    let params = &*params;
    (params.get_proc_address)(
        &params.ctx,
        CStr::from_ptr(name)
            .to_str()
            .expect("Could not convert function name to str"),
    )
}

impl<C> From<&OpenGLInitParams<C>> for libmpv_sys::mpv_opengl_init_params {
    fn from(val: &OpenGLInitParams<C>) -> Self {
        Self {
            get_proc_address: Some(gpa_wrapper::<OpenGLInitParams<C>>),
            get_proc_address_ctx: Box::into_raw(Box::new(val)) as *mut c_void,
            extra_exts: ptr::null(),
        }
    }
}

impl<C> From<&RenderParam<C>> for libmpv_sys::mpv_render_param {
    fn from(val: &RenderParam<C>) -> Self {
        let type_ = u32::from(val);
        let data = match val {
            RenderParam::Invalid => ptr::null_mut(),
            RenderParam::ApiType(api_type) => {
                let api_type = CString::new(*api_type).expect("Converting to API type to CString");
                api_type.as_ptr() as *mut c_void
            }
            RenderParam::InitParams(params) => {
                Box::into_raw(Box::new(libmpv_sys::mpv_opengl_init_params::from(params))) as *mut c_void
            }
            RenderParam::FBO(fbo) => Box::into_raw(Box::new(fbo)) as *mut c_void,
            RenderParam::FlipY(flip) => {
                Box::into_raw(if *flip { Box::new(1) } else { Box::new(0) }) as *mut c_void
            }
            RenderParam::Depth(depth) => Box::into_raw(Box::new(depth)) as *mut c_void,
            RenderParam::ICCProfile(bytes) => {
                Box::into_raw(bytes.clone().into_boxed_slice()) as *mut c_void
            }
            RenderParam::AmbientLight(lux) => Box::into_raw(Box::new(lux)) as *mut c_void,
            RenderParam::X11Display(ptr) => *ptr as *mut _,
            RenderParam::WaylandDisplay(ptr) => *ptr as *mut _,
            RenderParam::AdvancedControl(adv_ctrl) => {
                Box::into_raw(if *adv_ctrl { Box::new(1) } else { Box::new(0) }) as *mut c_void
            }
            RenderParam::NextFrameInfo(frame_info) => Box::into_raw(Box::new(frame_info.clone())) as *mut c_void,
            RenderParam::BlockForTargetTime(block) => {
                Box::into_raw(if *block { Box::new(1) } else { Box::new(0) }) as *mut c_void
            }
            RenderParam::SkipRendering(skip_rendering) => Box::into_raw(if *skip_rendering {
                Box::new(1)
            } else {
                Box::new(0)
            }) as *mut c_void,
        };
        Self { type_, data }
    }
}

impl RenderContext {
    pub(crate) fn new<C>(mpv: &libmpv_sys::mpv_handle, params: &Vec<RenderParam<C>>) -> Self {
        let mut raw_params: Vec<libmpv_sys::mpv_render_param> = Vec::new();

        for p in params.iter() {
            raw_params.push(p.into());
        }

        unimplemented!()
    }

    unsafe fn dealloc_renderparams(&self) {}
}
