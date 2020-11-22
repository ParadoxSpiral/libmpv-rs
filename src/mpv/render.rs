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

use crate::{mpv::mpv_err, Result, Error};
use std::collections::HashMap;
use std::convert::From;
use std::ffi::{c_void, CStr, CString};
use std::mem::MaybeUninit;
use std::ptr;
use libmpv_sys::{self, mpv_render_frame_info, mpv_render_param, mpv_opengl_init_params, mpv_render_context, mpv_handle};

type DeleterFn = unsafe fn(*mut c_void);

pub struct RenderContext {
    ctx: *mut mpv_render_context,
    // This is our little dirty bag of raw pointers that need specific
    // deallocation. The rendercontext typically took ownership of these when
    // renderparams were passed.
    raw_ptrs: HashMap<*mut c_void, DeleterFn>,
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

#[repr(u32)]
pub enum RenderFrameInfoFlag {
    Present = libmpv_sys::mpv_render_frame_info_flag_MPV_RENDER_FRAME_INFO_PRESENT,
    Redraw = libmpv_sys::mpv_render_frame_info_flag_MPV_RENDER_FRAME_INFO_REDRAW,
    Repeat = libmpv_sys::mpv_render_frame_info_flag_MPV_RENDER_FRAME_INFO_REPEAT,
    BlockVSync = libmpv_sys::mpv_render_frame_info_flag_MPV_RENDER_FRAME_INFO_BLOCK_VSYNC,
}

impl From<u64> for RenderFrameInfoFlag {
    // mpv_render_frame_info_flag is u32, but mpv_render_frame_info.flags is u64 o\
    fn from(val: u64) -> Self {
        let val = val as u32;
        match val {
            libmpv_sys::mpv_render_frame_info_flag_MPV_RENDER_FRAME_INFO_PRESENT => {
                RenderFrameInfoFlag::Present
            }
            libmpv_sys::mpv_render_frame_info_flag_MPV_RENDER_FRAME_INFO_REDRAW => RenderFrameInfoFlag::Redraw,
            libmpv_sys::mpv_render_frame_info_flag_MPV_RENDER_FRAME_INFO_REPEAT => RenderFrameInfoFlag::Repeat,
            libmpv_sys::mpv_render_frame_info_flag_MPV_RENDER_FRAME_INFO_BLOCK_VSYNC => {
                RenderFrameInfoFlag::BlockVSync
            }
            _ => panic!("Tried converting invalid value to RenderFrameInfoFlag"),
        }
    }
}

pub struct RenderFrameInfo {
    pub flags: RenderFrameInfoFlag,
    pub target_time: i64,
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

impl<C> From<&OpenGLInitParams<C>> for mpv_opengl_init_params {
    fn from(val: &OpenGLInitParams<C>) -> Self {
        Self {
            get_proc_address: Some(gpa_wrapper::<OpenGLInitParams<C>>),
            get_proc_address_ctx: Box::into_raw(Box::new(val)) as *mut c_void,
            extra_exts: ptr::null(),
        }
    }
}

impl<C> From<&RenderParam<C>> for mpv_render_param {
    fn from(val: &RenderParam<C>) -> Self {
        let type_ = u32::from(val);
        let data = match val {
            RenderParam::Invalid => ptr::null_mut(),
            RenderParam::ApiType(api_type) => {
                let api_type = CString::new(*api_type).expect("Converting to API type to CString");
                api_type.as_ptr() as *mut c_void
            }
            RenderParam::InitParams(params) => {
                Box::into_raw(Box::new(mpv_opengl_init_params::from(params))) as *mut c_void
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
            RenderParam::NextFrameInfo(frame_info) => {
                Box::into_raw(Box::new(frame_info.clone())) as *mut c_void
            }
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

unsafe fn free_void_data<T>(ptr: *mut c_void) {
    Box::<T>::from_raw(ptr as *mut T);
}

impl RenderContext {
    pub(crate) fn new<C>(mpv: &mut mpv_handle, params: &Vec<RenderParam<C>>) -> Result<Self> {
        let mut raw_params: Vec<mpv_render_param> = Vec::new();
        raw_params.reserve(params.len() + 1);
        let mut raw_ptrs: HashMap<*mut c_void, DeleterFn> = HashMap::new();

        for p in params.iter() {
            let raw_param: mpv_render_param = p.into();

            // The render params are type-erased after they are passed to mpv. This is where we last
            // know their real types, so we keep a deleter here.
            let deleter: Option<DeleterFn> = match p {
                RenderParam::InitParams(_) => Some(free_void_data::<OpenGLInitParams<C>>),
                RenderParam::FBO(_) => Some(free_void_data::<FBO>),
                RenderParam::FlipY(_) => Some(free_void_data::<i32>),
                RenderParam::Depth(_) => Some(free_void_data::<i32>),
                RenderParam::ICCProfile(_) => Some(free_void_data::<Box<[u8]>>),
                RenderParam::AmbientLight(_) => Some(free_void_data::<i32>),
                _ => None,
            };
            if let Some(deleter) = deleter {
                raw_ptrs.insert(raw_param.data, deleter);
            }

            raw_params.push(raw_param);
        }
        // the raw array must end with type = 0
        raw_params.push(mpv_render_param {
            type_: 0,
            data: ptr::null_mut(),
        });

        unsafe {
            let raw_array = Box::into_raw(raw_params.into_boxed_slice()) as *mut mpv_render_param;
            let ctx: mpv_render_context = MaybeUninit::uninit().assume_init();
            let ctx = Box::new(ctx);
            let mut ctx = Box::into_raw(ctx);
            mpv_err(
                Self { ctx, raw_ptrs },
                libmpv_sys::mpv_render_context_create(&mut ctx, &mut *mpv, raw_array),
            )
        }
    }

    pub fn set_parameter<C>(&self, param: &RenderParam<C>) -> Result<()> {
        unsafe {
            mpv_err(
                (),
                libmpv_sys::mpv_render_context_set_parameter(self.ctx, mpv_render_param::from(param)),
            )
        }
    }

    pub fn get_info<C>(&self, param: &RenderParam<C>) -> Result<RenderParam<C>> {
        let param = param.clone();
        let raw_param = mpv_render_param::from(param);
        let res = unsafe { libmpv_sys::mpv_render_context_get_info(self.ctx, raw_param) };
        if res == 0 {
            match param {
                RenderParam::NextFrameInfo(_) => {
                    let raw_frame_info = raw_param.data as *mut mpv_render_frame_info;
                    unsafe {
                        let raw_frame_info = *raw_frame_info;
                        return Ok(RenderParam::NextFrameInfo(Box::new(RenderFrameInfo {
                            flags: raw_frame_info.flags.into(),
                            target_time: raw_frame_info.target_time,
                        })));
                    }
                }
                _ => panic!("I don't know how to handle this info type."),
            }
        }
        Err(Error::Raw(res))
    }
}

impl Drop for RenderContext {
    fn drop(&mut self) {
        unsafe {
            for (ptr, deleter) in self.raw_ptrs.iter() {
                (deleter)(*ptr);
            }
        }
    }
}
