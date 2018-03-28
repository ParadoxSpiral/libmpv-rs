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

use super::*;
use super::mpv_err;
use super::super::raw::*;

use std::ffi::CStr;
use std::marker::PhantomData;
use std::panic;
use std::panic::{AssertUnwindSafe, RefUnwindSafe};
use std::ptr;
use std::os::raw as ctype;
use std::sync::atomic::Ordering;

impl Mpv {
    #[inline]
    /// Create a context with which opengl callback functions can be used.
    ///
    /// `vo` has to be set to `opengl-cb` for this to work properly.
    pub fn create_opengl_context<F, V>(&self, procaddr: F) -> Result<OpenGlContext<V>>
    where
        F: for<'a> Fn(&'a str) -> *const () + 'static,
    {
        if self.opengl_guard
            .compare_and_swap(false, true, Ordering::AcqRel)
        {
            bail!("Context already exists")
        } else {
            OpenGlContext::new(self.ctx, procaddr, PhantomData::<&Self>)
        }
    }
}

unsafe extern "C" fn get_proc_addr_wrapper<F>(
    cookie: *mut ctype::c_void,
    name: *const ctype::c_char,
) -> *mut ctype::c_void
where
    F: for<'a> Fn(&'a str) -> *const (),
{
    let fun = cookie as *mut F;

    let ret = panic::catch_unwind(AssertUnwindSafe(|| {
        let name = CStr::from_ptr(name).to_str().unwrap();
        (*fun)(name) as *mut () as *mut ctype::c_void
    }));
    if ret.is_ok() {
        ret.unwrap()
    } else {
        ptr::null_mut()
    }
}

#[allow(unused_must_use)]
unsafe extern "C" fn callback_update_wrapper<F, V>(cb_ctx: *mut ctype::c_void)
where
    F: for<'a> Fn(&'a V) + RefUnwindSafe + 'static,
    V: RefUnwindSafe,
{
    let data = cb_ctx as *mut (*const F, *mut V);

    panic::catch_unwind(|| {
        (*(*data).0)(&*(*data).1);
    });
}

/// Holds all state relevant to opengl callback functions.
/// It is created by calling `Mpv::create_opengl_context`.
///
/// # Safety
/// Mpv relies on correct and initialized OpenGL state.
pub struct OpenGlContext<'parent, V> {
    api_ctx: *mut mpv_opengl_cb_context,
    update_callback_data: Option<V>,
    _does_not_outlive: PhantomData<&'parent Mpv>,
}

unsafe impl<'parent, V> Send for OpenGlContext<'parent, V> {}
unsafe impl<'parent, V> Sync for OpenGlContext<'parent, V> {}

impl<'parent, V> Drop for OpenGlContext<'parent, V> {
    fn drop(&mut self) {
        unsafe { mpv_opengl_cb_uninit_gl(self.api_ctx) };
    }
}

impl<'parent, V> OpenGlContext<'parent, V> {
    fn new<F>(
        mpv_ctx: *mut mpv_handle,
        mut proc_addr: F,
        parent: PhantomData<&'parent Mpv>,
    ) -> Result<OpenGlContext<'parent, V>>
    where
        F: for<'a> Fn(&'a str) -> *const () + 'static,
    {
        let api_ctx = unsafe {
            mpv_get_sub_api(mpv_ctx, mpv_sub_api::MPV_SUB_API_OPENGL_CB)
                as *mut mpv_opengl_cb_context
        };
        assert!(!api_ctx.is_null());

        let proc_addr_ptr = &mut proc_addr as *mut F;

        mpv_err(
            OpenGlContext {
                api_ctx,
                update_callback_data: None,
                _does_not_outlive: parent,
            },
            unsafe {
                mpv_opengl_cb_init_gl(
                    api_ctx,
                    ptr::null(),
                    Some(get_proc_addr_wrapper::<F>),
                    proc_addr_ptr as *mut ctype::c_void,
                )
            },
        )
    }

    #[inline]
    /// Set the fbo that mpv will draw on, and start rendering.
    /// Passing `0` as `w` or `h` will choose the size of the fbo.
    /// If `h` is negative, the coordinate system is flipped.
    ///
    /// # Safety
    /// See [OpenGlContext](struct.OpenGlContext.html).
    pub unsafe fn draw(&self, fbo: ctype::c_int, w: usize, h: isize) -> Result<()> {
        mpv_err((), mpv_opengl_cb_draw(self.api_ctx, fbo, w as _, h as _))
    }

    #[inline]
    /// Tell the renderer that a frame was flipped.
    ///
    /// Note that calling this at least once informs libmpv that you will use this
    /// function. If you use it inconsistently, expect bad video playback.
    ///
    /// # Safety
    /// See [OpenGlContext](struct.OpenGlContext.html).
    pub unsafe fn report_flip(&self) -> Result<()> {
        mpv_err((), mpv_opengl_cb_report_flip(self.api_ctx, 0))
    }

    #[inline]
    /// Set the callback that notifies you when a new video frame is available, or if the
    /// video display configuration somehow changed and requires a redraw.
    ///
    /// # Safety
    /// Do not call any mpv API during the callback.
    pub unsafe fn set_update_callback<F>(&mut self, mut data: V, callback: F)
    where
        F: for<'a> Fn(&'a V) + RefUnwindSafe + 'static,
        V: RefUnwindSafe,
    {
        mpv_opengl_cb_set_update_callback(
            self.api_ctx,
            Some(callback_update_wrapper::<F, V>),
            &mut (&callback as *const F, &mut data as *mut V) as *mut (*const F, *mut V)
                as *mut ctype::c_void,
        );

        self.update_callback_data = Some(data);
    }
}
