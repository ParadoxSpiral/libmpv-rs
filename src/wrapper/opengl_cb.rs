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
use super::utils::mpv_err;
use super::super::raw::*;

use std::ffi::CStr;
use std::mem;
use std::panic;
use std::panic::AssertUnwindSafe;
use std::ptr;

unsafe extern "C" fn get_proc_addr_wrapper<F>(cookie: *mut libc::c_void,
                                           name: *const libc::c_char)
                                           -> *mut libc::c_void
    where F: for<'a> Fn(&'a str) -> *const ()
{
    let fun = cookie as *mut F;

    let ret = panic::catch_unwind(AssertUnwindSafe( || {
        let name = CStr::from_ptr(name).to_str().unwrap();
        (*fun)(name) as *mut () as *mut libc::c_void
    }));
    if ret.is_ok() {
        ret.unwrap()
    } else {
        ptr::null_mut()
    }
}

/// Holds all state relevant to opengl callback functions.
pub struct OpenGlState  {
    api_ctx: *mut MpvOpenGlCbContext,
    is_empty: bool,
}

impl OpenGlState {
    pub(crate) fn empty() -> OpenGlState {
        OpenGlState{
            api_ctx: ptr::null_mut(),
            is_empty: true,
        }
    }

    pub(crate) fn new<F>(mpv_ctx: *mut MpvHandle, mut proc_addr: F) -> Result<OpenGlState, Error>
        where F: for<'a> Fn(&'a str) -> *const () + 'static
    {
        let api_ctx = unsafe {
            mpv_get_sub_api(mpv_ctx, MpvSubApi::OpenglCb) as *mut MpvOpenGlCbContext
        };
        debug_assert!(!api_ctx.is_null());

        let proc_addr_ptr = &mut proc_addr as *mut F;

        let ret = mpv_err(
            OpenGlState {
                api_ctx: api_ctx,
                is_empty: false,
            },
            unsafe {
                    mpv_opengl_cb_init_gl(api_ctx, ptr::null(),
                                          get_proc_addr_wrapper::<F>,
                                          proc_addr_ptr as *mut libc::c_void)
            }
        );
        if ret.is_ok() {
            mem::forget(proc_addr);
        }
        ret
    }
}

impl Drop for OpenGlState {
    #[inline]
    fn drop(&mut self) {
        if !self.is_empty {
            unsafe {
                mpv_opengl_cb_uninit_gl(self.api_ctx);
            }
        }
    }
}
