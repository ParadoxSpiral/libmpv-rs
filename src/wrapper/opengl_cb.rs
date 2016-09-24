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

unsafe extern "C" fn get_proc_addr_wrapper(cookie: *mut libc::c_void,
                                           name: *const libc::c_char)
                                           -> *mut libc::c_void
{
    let fun = cookie as *mut for<'a> fn(&'a str) -> *const ();

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
pub struct OpenGlState<F> where F: for<'a> Fn(&'a str) -> *const () {
    api_ctx: *mut MpvOpenGlCbContext,
    proc_addr: *mut F,
    is_empty: bool,
}

impl<F> OpenGlState<F> where F: for<'a> Fn(&'a str) -> *const () {
    pub(crate) fn empty() -> OpenGlState<F> {
        OpenGlState{
            api_ctx: ptr::null_mut(),
            proc_addr: ptr::null_mut(),
            is_empty: true,
        }
    }

    pub(crate) fn new(mpv_ctx: *mut MpvHandle, proc_addr: F) -> Result<OpenGlState<F>, Error> {
        let api_ctx = unsafe {
            mpv_get_sub_api(mpv_ctx, MpvSubApi::OpenglCb) as *mut MpvOpenGlCbContext
        };
        debug_assert!(!api_ctx.is_null());

        let ret = mpv_err(
            OpenGlState {
                api_ctx: api_ctx,
                proc_addr: (&mut proc_addr) as *mut F,
                is_empty: false
            },
            unsafe {
                    mpv_opengl_cb_init_gl(api_ctx, ptr::null(),
                                          get_proc_addr_wrapper,
                                          (&mut proc_addr) as *mut F
                                                    as *mut for<'a> fn(&'a str) -> *const ()
                                                    as *mut libc::c_void)
            }
        );
        mem::forget(proc_addr);
        ret
    }
}

impl<F> Drop for OpenGlState<F> where F: for<'a> Fn(&'a str) -> *const () {
    #[inline]
    fn drop(&mut self) {
        if !self.is_empty {
            unsafe {
                mpv_opengl_cb_uninit_gl(self.api_ctx);
                Box::from_raw(self.proc_addr);
            }
        }
    }
}
