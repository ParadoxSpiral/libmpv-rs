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

use libc;

use super::*;
use super::mpv_err;
use super::super::raw::*;

use std::ffi::CStr;
use std::panic;
use std::panic::RefUnwindSafe;
use std::ptr;

unsafe extern "C" fn get_proc_addr_wrapper<F>(cookie: *mut libc::c_void,
                                           name: *const libc::c_char)
                                           -> *mut libc::c_void
    where F: for<'a> Fn(&'a str) -> *const () + RefUnwindSafe
{
    let fun = cookie as *mut F;

    let ret = panic::catch_unwind(|| {
        let name = CStr::from_ptr(name).to_str().unwrap();
        (*fun)(name) as *mut () as *mut libc::c_void
    });
    if ret.is_ok() {
        ret.unwrap()
    } else {
        ptr::null_mut()
    }
}

#[allow(unused_must_use)]
unsafe extern "C" fn callback_update_wrapper<F, V>(cb_ctx: *mut libc::c_void)
    where F: for<'a> Fn(&'a V) + RefUnwindSafe + 'static, V: RefUnwindSafe
{
    let data = cb_ctx as *mut (*const F, *mut V);

    panic::catch_unwind(|| {
        (*(*data).0)(&*(*data).1);
    });
}

/// Holds all state relevant to opengl callback functions.
///
/// # Safety
/// Mpv relies on correct and initialized OpenGL state.
pub struct OpenGlState<V: RefUnwindSafe> {
    pub(crate) api_ctx: *mut MpvOpenGlCbContext,
    pub(crate) update_callback_data: V,
}

impl<V: RefUnwindSafe> OpenGlState<V> {
    pub(crate) fn empty() -> OpenGlState<V> {
        OpenGlState{
            api_ctx: ptr::null_mut(),
            update_callback_data: unsafe{ mem::zeroed() },
        }
    }

    pub(crate) fn new<F>(mpv_ctx: *mut MpvHandle, mut proc_addr: F) -> Result<OpenGlState<V>>
        where F: for<'a> Fn(&'a str) -> *const () + RefUnwindSafe + 'static
    {
        let api_ctx = unsafe {
            mpv_get_sub_api(mpv_ctx, MpvSubApi::OpenglCb) as *mut MpvOpenGlCbContext
        };
        debug_assert!(!api_ctx.is_null());

        let proc_addr_ptr = &mut proc_addr as *mut F;

        mpv_err(
            OpenGlState {
                api_ctx: api_ctx,
                update_callback_data: unsafe{ mem::zeroed() },
            },
            unsafe {
                    mpv_opengl_cb_init_gl(api_ctx, ptr::null(),
                                          get_proc_addr_wrapper::<F>,
                                          proc_addr_ptr as *mut libc::c_void)
            }
        )
    }

    #[inline]
    /// Set the fbo that mpv will draw on, and start rendering.
    /// Passing `0` as `w` or `h` will choose the size of the fbo.
    pub unsafe fn set_draw_target(&self, fbo: libc::c_int, w: usize, h: usize)
        -> Result<()>
    {
        mpv_err((), mpv_opengl_cb_draw(self.api_ctx, fbo, w as _, h as _))
    }

    #[inline]
    /// Tell the renderer that a frame was flipped.
    ///
    /// Note that calling this at least once informs libmpv that you will use this
    /// function. If you use it inconsistently, expect bad video playback.
    // TODO: The time parameter is currently not given, because it is ignored.
    pub unsafe fn report_flip(&self) -> Result<()> {
        mpv_err((), mpv_opengl_cb_report_flip(self.api_ctx, 0))
    }

    #[inline]
    /// Set the callback that notifies you when a new video frame is available, or if the
    /// video display configuration somehow changed and requires a redraw.
    ///
    /// # Safety
    /// Do not call any mpv API during the callback
    pub unsafe fn set_update_callback<F>(&mut self, mut data: V, callback: F)
        where F: for<'a> Fn(&'a V) + RefUnwindSafe + 'static, V: RefUnwindSafe
    {
        mpv_opengl_cb_set_update_callback(self.api_ctx,
                                          callback_update_wrapper::<F, V>,
                                          &mut (&callback as *const F, &mut data as *mut V)
                                            as *mut (*const F, *mut V)
                                            as *mut libc::c_void);
        
        self.update_callback_data = data;
    }
}
