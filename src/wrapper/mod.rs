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

mod errors {
    #![allow(missing_docs)]
    use super::events::Event;
    use super::super::raw::MpvError;
    use std::ffi::NulError;
    use std::str::Utf8Error;

    // FIXME: Once error_chain issue 134 is solved, this should derive Clone, and use RCs
    // instead of `Box`es. Remove temp impl Clone below then.
    error_chain!{
        foreign_links {
            Nul(NulError);
            Utf8(Utf8Error);
            Native(MpvError);
        }

        errors {
            Loadfiles(index: usize, error: Box<Error>) {
                description("Command failed during a `loadfiles` call.")
            }
            AlreadyObserved(e: Box<Event>) {
                description("This event is already being observed by another `EventIter`.")
            }
            InvalidArgument {
                description("An invalid argument was passed to an mpv API")
            }
            VersionMismatch(linked: u32, loaded: u32) {
                description("The library was compiled against a different mpv version than what is present on the system.")
            }
            ContextExists {
                description("An opengl or protocol context has already been created.")
            }
            EventsDisabled {
                description("Events are disabled.")
            }
            InvalidUtf8 {
                description("An unspecified error during utf-8 validity checking ocurred.")
            }
            Null {
                description("Mpv returned null while creating the core.")
            }
        }
    }

    impl Clone for Error {
        #[allow(match_ref_pats)]
        fn clone(&self) -> Error {
            match self.kind() {
                &ErrorKind::Msg(ref e) => ErrorKind::Msg(e.clone()).into(),
                &ErrorKind::Nul(ref e) => ErrorKind::Nul(e.clone()).into(),
                &ErrorKind::Utf8(ref e) => ErrorKind::Utf8(*e).into(),
                &ErrorKind::Native(ref e) => ErrorKind::Native(*e).into(),
                &ErrorKind::Loadfiles(ref idx, ref err) => {
                    ErrorKind::Loadfiles(*idx, err.clone()).into()
                }
                &ErrorKind::AlreadyObserved(ref e) => ErrorKind::AlreadyObserved(e.clone()).into(),
                &ErrorKind::InvalidArgument => ErrorKind::InvalidArgument.into(),
                &ErrorKind::VersionMismatch(ref li, ref lo) => {
                    ErrorKind::VersionMismatch(*li, *lo).into()
                }
                &ErrorKind::ContextExists => ErrorKind::ContextExists.into(),
                &ErrorKind::EventsDisabled => ErrorKind::EventsDisabled.into(),
                &ErrorKind::InvalidUtf8 => ErrorKind::InvalidUtf8.into(),
                &ErrorKind::Null => ErrorKind::Null.into(),
            }
        }
    }
}

pub use self::errors::*;

#[cfg(unix)]
macro_rules! mpv_cstr_to_string {
    ($cstr: expr) => (
        if let Some(v) = OsStr::from_bytes($cstr.to_bytes()).to_str() {
            // Not sure why the type isn't inferred
            let r: Result<String> = Ok(v.to_owned());
            r
        } else {
            Err(ErrorKind::InvalidUtf8.into())
        }
    )
}

#[cfg(all(not(unix)))]
macro_rules! mpv_cstr_to_string {
    ($cstr: expr) => (
        String::from_utf8($cstr.to_bytes())
    )
}

/// Contains event related abstractions
pub mod events;
/// Contains abstractions to define custom protocol handlers.
pub mod protocol;
/// Contains abstractions to use the opengl callback interface.
pub mod opengl_cb;

use enum_primitive::FromPrimitive;
use libc;
use parking_lot::{Condvar, Mutex};

use super::raw::*;
use events::*;
use events::event_callback;
use protocol::*;
use opengl_cb::*;

use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::marker::PhantomData;
use std::mem;
use std::ops::Deref;
use std::panic::RefUnwindSafe;
use std::ptr;
use std::time::Duration;
#[cfg(unix)]
use std::ffi::OsStr;
#[cfg(unix)]
use std::os::unix::ffi::OsStrExt;

fn mpv_err<T>(ret: T, err_val: libc::c_int) -> Result<T> {
    if err_val == 0 {
        Ok(ret)
    } else {
        Err(ErrorKind::Native(MpvError::from_i32(err_val).unwrap()).into())
    }
}

#[allow(missing_docs)]
/// This trait describes which types are allowed to be passed to getter mpv APIs.
pub unsafe trait GetData: Clone {
    #[doc(hidden)]
    fn get_from_c_void<T, F: FnMut(*mut libc::c_void) -> Result<T>>(mut fun: F) -> Result<Self> {
        let mut ptr = unsafe { mem::zeroed() };
        let _ = fun(&mut ptr as *mut Self as _)?;
        Ok(ptr)
    }
    fn get_format() -> Format;
}

#[allow(missing_docs)]
/// This trait describes which types are allowed to be passed to setter mpv APIs.
pub unsafe trait SetData: Clone {
    #[doc(hidden)]
    fn call_as_c_void<T, F: FnMut(*mut libc::c_void) -> Result<T>>(mut self,
                                                                   mut fun: F)
                                                                   -> Result<T> {
        fun(&mut self as *mut Self as _)
    }
    fn get_format() -> Format;
}

unsafe impl GetData for f64 {
    #[inline]
    fn get_format() -> Format {
        Format::Double
    }
}

unsafe impl SetData for f64 {
    #[inline]
    fn get_format() -> Format {
        Format::Double
    }
}

unsafe impl GetData for i64 {
    #[inline]
    fn get_format() -> Format {
        Format::Int64
    }
}

unsafe impl SetData for i64 {
    #[inline]
    fn get_format() -> Format {
        Format::Int64
    }
}

unsafe impl GetData for bool {
    #[inline]
    fn get_from_c_void<T, F: FnMut(*mut libc::c_void) -> Result<T>>(mut fun: F) -> Result<bool> {
        let ptr = unsafe { &mut mem::zeroed() } as *mut i64;
        let _ = fun(ptr as _)?;
        Ok(match unsafe { *ptr } {
               0 => false,
               1 => true,
               _ => unreachable!(),
           })
    }

    #[inline]
    fn get_format() -> Format {
        Format::Flag
    }
}

unsafe impl SetData for bool {
    #[inline]
    fn call_as_c_void<T, F: FnMut(*mut libc::c_void) -> Result<T>>(self, mut fun: F) -> Result<T> {
        let mut cpy: i64 = if self { 1 } else { 0 };
        fun(&mut cpy as *mut i64 as *mut _)
    }

    #[inline]
    fn get_format() -> Format {
        Format::Flag
    }
}

unsafe impl GetData for String {
    #[inline]
    fn get_from_c_void<T, F: FnMut(*mut libc::c_void) -> Result<T>>(mut fun: F) -> Result<String> {
        let ptr = &mut ptr::null();
        let _ = fun(ptr as *mut *const libc::c_char as _)?;

        let ret = mpv_cstr_to_string!(unsafe { CStr::from_ptr(*ptr) });
        unsafe { mpv_free(*ptr as *mut _) };
        Ok(ret?)
    }

    #[inline]
    fn get_format() -> Format {
        Format::String
    }
}

unsafe impl SetData for String {
    #[inline]
    fn call_as_c_void<T, F: FnMut(*mut libc::c_void) -> Result<T>>(self, mut fun: F) -> Result<T> {
        let string = CString::new(self)?;
        fun((&mut string.as_ptr()) as *mut *const libc::c_char as *mut _)
    }

    #[inline]
    fn get_format() -> Format {
        Format::String
    }
}

unsafe impl<'a> SetData for &'a str {
    #[inline]
    fn call_as_c_void<T, F: FnMut(*mut libc::c_void) -> Result<T>>(self, mut fun: F) -> Result<T> {
        let string = CString::new(self)?;
        fun((&mut string.as_ptr()) as *mut *const libc::c_char as *mut _)
    }

    #[inline]
    fn get_format() -> Format {
        Format::String
    }
}

#[derive(Clone)]
/// An `OsdString` can only be used by getter functions, and is subject to [property expansion](https://mpv.io/manual/master/#property-expansion).
pub struct OsdString(String);
impl Deref for OsdString {
    type Target = String;

    fn deref(&self) -> &String {
        &self.0
    }
}

unsafe impl<'a> GetData for OsdString {
    #[inline]
    fn get_from_c_void<T, F: FnMut(*mut libc::c_void) -> Result<T>>(fun: F) -> Result<OsdString> {
        Ok(OsdString(String::get_from_c_void(fun)?))
    }

    #[inline]
    fn get_format() -> Format {
        Format::OsdString
    }
}

unsafe impl<'a> SetData for OsdString {
    #[inline]
    fn call_as_c_void<T, F: FnMut(*mut libc::c_void) -> Result<T>>(self, fun: F) -> Result<T> {
        self.0.call_as_c_void(fun)
    }

    #[inline]
    fn get_format() -> Format {
        Format::OsdString
    }
}

#[allow(missing_docs)]
#[derive(Debug, Clone)]
/// Subset of `MpvFormat` used by the public API.
pub enum Format {
    String,
    OsdString,
    Flag,
    Int64,
    Double,
}

impl Format {
    fn as_mpv_format(&self) -> MpvFormat {
        match *self {
            Format::String => MpvFormat::String,
            Format::OsdString => MpvFormat::OsdString,
            Format::Flag => MpvFormat::Flag,
            Format::Int64 => MpvFormat::Int64,
            Format::Double => MpvFormat::Double,
        }
    }
}

impl MpvError {
    fn as_val(&self) -> libc::c_int {
        *self as libc::c_int
    }

    #[inline]
    /// Returns the associated error string.
    pub fn error_string(&self) -> &str {
        let raw = unsafe { mpv_error_string(self.as_val()) };
        unsafe { CStr::from_ptr(raw) }.to_str().unwrap()
    }
}

impl MpvFormat {
    fn as_val(self) -> libc::c_int {
        self as _
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
/// How a `File` is inserted into the playlist.
pub enum FileState {
    /// Replace the current track.
    Replace,
    /// Append to the current playlist.
    Append,
    /// If current playlist is empty: play, otherwise append to playlist.
    AppendPlay,
}

impl FileState {
    fn val(&self) -> &str {
        match *self {
            FileState::Replace => "replace",
            FileState::Append => "append",
            FileState::AppendPlay => "append-play",
        }
    }
}

/// An mpv instance from which `Client`s can be spawned.
pub struct Parent {
    ctx: *mut MpvHandle,
    events: bool,
    ev_iter_notification: Option<Box<(Mutex<bool>, Condvar)>>,
    ev_to_observe: Option<Mutex<Vec<Event>>>,
    ev_to_observe_properties: Option<Mutex<HashMap<String, libc::uint64_t>>>,
    ev_observed: Option<Mutex<Vec<Event>>>,
    protocols_guard: Mutex<()>,
    opengl_guard: Mutex<()>,
}

/// A client of a `Parent`.
pub struct Client<'parent> {
    ctx: *mut MpvHandle,
    events: bool,
    ev_iter_notification: Option<Box<(Mutex<bool>, Condvar)>>,
    ev_to_observe: Option<Mutex<Vec<Event>>>,
    ev_observed: Option<Mutex<Vec<Event>>>,
    ev_to_observe_properties: Option<Mutex<HashMap<String, libc::uint64_t>>>,
    _does_not_outlive: PhantomData<&'parent Parent>,
}

unsafe impl Send for Parent {}
unsafe impl Sync for Parent {}
unsafe impl<'parent> Send for Client<'parent> {}
unsafe impl<'parent> Sync for Client<'parent> {}

impl Drop for Parent {
    #[inline]
    fn drop(&mut self) {
        unsafe {
            mpv_terminate_destroy(self.ctx());
        }
    }
}

impl<'parent> Drop for Client<'parent> {
    #[inline]
    fn drop(&mut self) {
        unsafe {
            mpv_detach_destroy(self.ctx());
        }
    }
}

impl Parent {
    #[inline]
    /// Create a new `Parent`.
    /// The default settings can be probed by running: `$ mpv --show-profile=libmpv`
    pub fn new(events: bool) -> Result<Parent> {
        let api_version = unsafe { mpv_client_api_version() };
        if super::MPV_CLIENT_API_VERSION != api_version {
            return Err(ErrorKind::VersionMismatch(super::MPV_CLIENT_API_VERSION, api_version)
                           .into());
        }

        let ctx = unsafe { mpv_create() };
        if ctx.is_null() {
            return Err(ErrorKind::Null.into());
        }

        let (ev_iter_notification, ev_to_observe, ev_to_observe_properties, ev_observed) = {
            if events {
                let ev_iter_notification = Box::new((Mutex::new(false), Condvar::new()));
                unsafe {
                    mpv_set_wakeup_callback(ctx,
                                            event_callback,
                                            &ev_iter_notification.1 as *const Condvar as
                                            *mut Condvar as
                                            *mut _);
                }

                (Some(ev_iter_notification),
                 Some(Mutex::new(Vec::with_capacity(10))),
                 Some(Mutex::new(HashMap::with_capacity(10))),
                 Some(Mutex::new(Vec::with_capacity(15))))
            } else {
                (None, None, None, None)
            }
        };

        mpv_err((), unsafe { mpv_initialize(ctx) })
            .or_else(|err| {
                         unsafe { mpv_terminate_destroy(ctx) };
                         Err(err)
                     })?;

        Ok(Parent {
               ctx: ctx,
               events: events,
               ev_iter_notification: ev_iter_notification,
               ev_to_observe: ev_to_observe,
               ev_to_observe_properties: ev_to_observe_properties,
               ev_observed: ev_observed,
               protocols_guard: Mutex::new(()),
               opengl_guard: Mutex::new(()),
           })
    }

    #[inline]
    /// Create a client with `name`, that is connected to the core of `self`, but has its own queue
    /// for API events and such.
    pub fn new_client(&self, name: &str, events: bool) -> Result<Client> {
        let ctx = unsafe {
            let name = CString::new(name)?;
            mpv_create_client(self.ctx(), name.as_ptr())
        };

        let (ev_iter_notification, ev_to_observe, ev_to_observe_properties, ev_observed) = {
            if events {
                let mut ev_iter_notification = Box::new((Mutex::new(false), Condvar::new()));
                unsafe {
                    mpv_set_wakeup_callback(ctx,
                                            event_callback,
                                            (&mut ev_iter_notification.1) as *mut Condvar as
                                            *mut _);
                }

                (Some(ev_iter_notification),
                 Some(Mutex::new(Vec::with_capacity(10))),
                 Some(Mutex::new(HashMap::with_capacity(10))),
                 Some(Mutex::new(Vec::with_capacity(15))))
            } else {
                (None, None, None, None)
            }
        };

        Ok(Client {
               ctx: ctx,
               events: events,
               ev_iter_notification: ev_iter_notification,
               ev_to_observe: ev_to_observe,
               ev_to_observe_properties: ev_to_observe_properties,
               ev_observed: ev_observed,
               _does_not_outlive: PhantomData::<&Self>,
           })
    }

    #[inline]
    /// Create a context with which opengl callback functions can be used.
    ///
    /// `vo` has to be set to `opengl-cb` for this to work properly.
    pub fn create_opengl_context<F, V>(&self, procaddr: F) -> Result<OpenGlState<V>>
        where F: for<'a> Fn(&'a str) -> *const () + 'static
    {
        let guard = self.opengl_guard.try_lock();

        if guard.is_none() {
            Err(ErrorKind::ContextExists.into())
        } else {
            Ok(OpenGlState::new(self.ctx, procaddr, guard.unwrap(), PhantomData::<&Self>)?)
        }
    }

    #[inline]
    /// Create a context with which custom protocols can be registered.
    pub fn create_protocol_context<T, U>(&self, capacity: usize) -> Result<ProtocolContext<T, U>>
        where T: RefUnwindSafe,
              U: RefUnwindSafe
    {
        let guard = self.protocols_guard.try_lock();

        if guard.is_none() {
            Err(ErrorKind::ContextExists.into())
        } else {
            Ok(ProtocolContext::new(self.ctx, capacity, guard.unwrap(), PhantomData::<&Self>))
        }
    }
}

impl<'parent> Client<'parent> {
    #[inline]
    /// Returns the name associated with `self`.
    pub fn name(&self) -> Result<String> {
        mpv_cstr_to_string!(unsafe { CStr::from_ptr(mpv_client_name(self.ctx())) })
    }
}

#[allow(missing_docs)]
/// Core functionality that is supported by both `Client` and `Parent`.
pub trait MpvInstance: Sized {
    // FIXME: These can go once `Associated Fields` lands
    fn ctx(&self) -> *mut MpvHandle;
    fn events(&self) -> bool;
    fn ev_iter_notification(&self) -> &Option<Box<(Mutex<bool>, Condvar)>>;
    fn ev_to_observe(&self) -> &Option<Mutex<Vec<Event>>>;
    fn ev_to_observe_properties(&self) -> &Option<Mutex<HashMap<String, libc::uint64_t>>>;
    fn ev_observed(&self) -> &Option<Mutex<Vec<Event>>>;

    #[inline]
    /// Load a configuration file. The path has to be absolute, and a file.
    fn load_config(&self, path: &str) -> Result<()> {
        let file = CString::new(path)?.into_raw();
        let ret = mpv_err((), unsafe { mpv_load_config_file(self.ctx(), file) });
        unsafe { CString::from_raw(file) };
        ret
    }

    #[inline]
    /// Observe given `Event`s via an `EventIter`.
    fn observe_events(&self, events: &[Event]) -> Result<EventIter<Self>> {
        if !self.events() {
            return Err(ErrorKind::EventsDisabled.into());
        }

        let mut observe = self.ev_to_observe().as_ref().unwrap().lock();
        let mut properties = self.ev_to_observe_properties()
            .as_ref()
            .unwrap()
            .lock();

        let len = events.len();
        // FIXME: This can be alloca'ed once the RFC is implemented
        let mut ids = Vec::with_capacity(len);
        let mut evs = Vec::with_capacity(len);
        let mut props = Vec::with_capacity(len);
        for elem in events {
            if let Event::PropertyChange(ref v) = *elem {
                if properties.contains_key(&v.0) {
                    return Err(ErrorKind::AlreadyObserved(Box::new(elem.clone())).into());
                } else {
                    mpv_err((),
                            unsafe { mpv_request_event(self.ctx(), elem.as_id(), 1) })?;
                    props.push(v);
                    ids.push(elem.as_id());
                    evs.push(elem.clone());
                }
            } else {
                for id in &*observe {
                    if elem.as_id() == id.as_id() {
                        return Err(ErrorKind::AlreadyObserved(Box::new(elem.clone())).into());
                    }
                }

                if let Event::LogMessage { level: lvl, .. } = *elem {
                    let min_level = CString::new(lvl.as_str())?;
                    mpv_err((),
                            unsafe { mpv_request_log_messages(self.ctx(), min_level.as_ptr()) })?;
                }

                mpv_err((),
                        unsafe { mpv_request_event(self.ctx(), elem.as_id(), 1) })?;
                ids.push(elem.as_id());
                evs.push(elem.clone());
            }
        }

        let mut props_ins = Vec::with_capacity(len);
        let start_id = properties.len();
        for (i, elem) in props.iter().enumerate() {
            let name = CString::new(&elem.0[..])?;
            let err = mpv_err((), unsafe {
                mpv_observe_property(self.ctx(),
                                     (start_id + i) as _,
                                     name.as_ptr(),
                                     elem.1.format() as _)
            });
            if err.is_err() {
                for (_, id) in props_ins {
                    // Ignore errors.
                    unsafe { mpv_unobserve_property(self.ctx(), id) };
                }
                return Err(err.unwrap_err());
            }
            props_ins.push((elem.0.clone(), (start_id + i) as _));
        }
        observe.extend(evs.clone());
        properties.extend(props_ins);

        Ok(EventIter {
               ctx: self.ctx(),
               first_iteration: true,
               notification: self.ev_iter_notification().as_ref().unwrap(),
               all_to_observe: self.ev_to_observe().as_ref().unwrap(),
               all_to_observe_properties: self.ev_to_observe_properties().as_ref().unwrap(),
               local_to_observe: evs,
               all_observed: self.ev_observed().as_ref().unwrap(),
               _does_not_outlive: PhantomData::<&Self>,
           })
    }

    #[inline]
    /// Send a command to the `Mpv` instance. This uses `mpv_command_string` internally,
    /// so that the syntax is the same as described in the [manual for the input.conf]
    /// (https://mpv.io/manual/master/#list-of-input-commands).
    ///
    /// Note that you may have to escape strings with `""` when they contain spaces.
    ///
    /// # Safety
    /// This method is unsafe because arbitrary code may be executed resulting in UB and more.
    unsafe fn command(&self, name: &str, args: &[&str]) -> Result<()> {
        let mut cmd = String::with_capacity(name.len() +
                                            args.iter().fold(0, |acc, e| acc + e.len() + 1));
        cmd.push_str(name);

        for elem in args {
            cmd.push_str(" ");
            cmd.push_str(elem);
        }
        let raw = CString::new(cmd)?;

        mpv_err((), mpv_command_string(self.ctx(), raw.as_ptr()))
    }

    #[inline]
    /// Set the value of a property.
    fn set_property<T: SetData>(&self, name: &str, data: T) -> Result<()> {
        let name = CString::new(name)?;
        let format = T::get_format().as_mpv_format().as_val();
        data.call_as_c_void(|ptr| {
                                mpv_err((), unsafe {
                mpv_set_property(self.ctx(), name.as_ptr(), format, ptr)
            })
                            })
    }

    #[inline]
    /// Get the value of a property.
    fn get_property<T: GetData>(&self, name: &str) -> Result<T> {
        let name = CString::new(name)?;

        let format = T::get_format().as_mpv_format().as_val();
        T::get_from_c_void(|ptr| {
                               mpv_err((), unsafe {
                mpv_get_property(self.ctx(), name.as_ptr(), format, ptr)
            })
                           })
    }

    #[inline]
    /// Internal time in microseconds, this has an arbitrary offset, and will never go backwards.
    ///
    /// This can be called at any time, even if it was stated that no API function should be called.
    fn get_internal_time(&self) -> i64 {
        unsafe { mpv_get_time_us(self.ctx()) }
    }

    // --- Convenience property functions ---
    //

    #[inline]
    /// Add -or subtract- any value from a property. Over/underflow clamps to max/min.
    fn add_property(&self, property: &str, value: isize) -> Result<()> {
        unsafe { self.command("add", &[property, &format!("{}", value)]) }
    }

    #[inline]
    /// Cycle through a given property. `up` specifies direction. On
    /// overflow, set the property back to the minimum, on underflow set it to the maximum.
    fn cycle_property(&self, property: &str, up: bool) -> Result<()> {
        unsafe { self.command("cycle", &[property, if up { "up" } else { "down" }]) }
    }

    #[inline]
    /// Multiply any property with any positive factor.
    fn multiply_property(&self, property: &str, factor: usize) -> Result<()> {
        unsafe { self.command("multiply", &[property, &format!("{}", factor)]) }
    }

    #[inline]
    /// Pause playback at runtime.
    fn pause(&self) -> Result<()> {
        self.set_property("pause", true)
    }

    #[inline]
    /// Unpause playback at runtime.
    fn unpause(&self) -> Result<()> {
        self.set_property("pause", false)
    }

    // --- Convenience command functions ---
    //

    // --- Seek functions ---
    //

    #[inline]
    /// Seek forward relatively from current position at runtime.
    /// This is less exact than `seek_absolute`, see [mpv manual]
    /// (https://mpv.io/manual/master/#command-interface-
    /// [relative|absolute|absolute-percent|relative-percent|exact|keyframes]).
    fn seek_forward(&self, time: &Duration) -> Result<()> {
        unsafe { self.command("seek", &[&format!("{}", time.as_secs()), "relative"]) }
    }

    #[inline]
    /// See `seek_forward`.
    fn seek_backward(&self, time: &Duration) -> Result<()> {
        unsafe { self.command("seek", &[&format!("-{}", time.as_secs()), "relative"]) }
    }

    #[inline]
    /// Seek to a given absolute time.
    fn seek_absolute(&self, time: &Duration) -> Result<()> {
        unsafe { self.command("seek", &[&format!("{}", time.as_secs()), "absolute"]) }
    }

    #[inline]
    /// Seek to a given relative percent position (may be negative).
    /// If `percent` of the playtime is bigger than the remaining playtime, the next file is played.
    /// out of bounds values are clamped to either 0 or 100.
    fn seek_percent(&self, percent: isize) -> Result<()> {
        unsafe { self.command("seek", &[&format!("{}", percent), "relative-percent"]) }
    }

    #[inline]
    /// Seek to the given percentage of the playtime.
    fn seek_percent_absolute(&self, percent: usize) -> Result<()> {
        unsafe { self.command("seek", &[&format!("{}", percent), "relative-percent"]) }
    }

    #[inline]
    /// Revert the previous `seek_` call, can also revert itself.
    fn seek_revert(&self) -> Result<()> {
        unsafe { self.command("revert-seek", &[]) }
    }

    #[inline]
    /// Mark the current position as the position that will be seeked to by `seek_revert`.
    fn seek_revert_mark(&self) -> Result<()> {
        unsafe { self.command("revert-seek", &["mark"]) }
    }

    #[inline]
    /// Seek exactly one frame, and pause.
    /// Noop on audio only streams.
    fn seek_frame(&self) -> Result<()> {
        unsafe { self.command("frame-step", &[]) }
    }

    #[inline]
    /// See `seek_frame`.
    /// [Note performance considerations.](https://mpv.io/manual/master/#command-interface-frame-back-step)
    fn seek_frame_backward(&self) -> Result<()> {
        unsafe { self.command("frame-back-step", &[]) }
    }

    // --- Screenshot functions ---
    //

    #[inline]
    /// "Save the video image, in its original resolution, and with subtitles.
    /// Some video outputs may still include the OSD in the output under certain circumstances.".
    ///
    /// "[O]ptionally save it to a given file. The format of the file will be
    /// guessed by the extension (and --screenshot-format is ignored - the behaviour when the
    /// extension is missing or unknown is arbitrary). If the file already exists, it's overwritten.
    /// Like all input command parameters, the filename is subject to property expansion as
    /// described in [Property Expansion](https://mpv.io/manual/master/#property-expansion)."
    fn screenshot_subtitles<'a, A: Into<Option<&'a str>>>(&self, path: A) -> Result<()> {
        if let Some(path) = path.into() {
            unsafe { self.command("screenshot", &[&format!("\"{}\"", path), "subtitles"]) }
        } else {
            unsafe { self.command("screenshot", &["subtitles"]) }
        }
    }

    #[inline]
    /// "Like subtitles, but typically without OSD or subtitles. The exact behavior depends on the selected
    /// video output."
    fn screenshot_video<'a, A: Into<Option<&'a str>>>(&self, path: A) -> Result<()> {
        if let Some(path) = path.into() {
            unsafe { self.command("screenshot", &[&format!("\"{}\"", path), "video"]) }
        } else {
            unsafe { self.command("screenshot", &["video"]) }
        }
    }

    #[inline]
    /// "Save the contents of the mpv window. Typically scaled, with OSD and subtitles. The exact
    /// behaviour depends on the selected video output, and if no support is available,
    /// this will act like video.".
    fn screenshot_window<'a, A: Into<Option<&'a str>>>(&self, path: A) -> Result<()> {
        if let Some(path) = path.into() {
            unsafe { self.command("screenshot", &[&format!("\"{}\"", path), "window"]) }
        } else {
            unsafe { self.command("screenshot", &["window"]) }
        }
    }

    // --- Playlist functions ---
    //

    #[inline]
    /// Play the next item of the current playlist.
    /// Does nothing if the current item is the last item.
    fn playlist_next_weak(&self) -> Result<()> {
        unsafe { self.command("playlist-next", &["weak"]) }
    }

    #[inline]
    /// Play the next item of the current playlist.
    /// Terminates playback if the current item is the last item.
    fn playlist_next_force(&self) -> Result<()> {
        unsafe { self.command("playlist-next", &["force"]) }
    }

    #[inline]
    /// See `playlist_next_weak`.
    fn playlist_previous_weak(&self) -> Result<()> {
        unsafe { self.command("playlist-previous", &["weak"]) }
    }

    #[inline]
    /// See `playlist_next_force`.
    fn playlist_previous_force(&self) -> Result<()> {
        unsafe { self.command("playlist-previous", &["force"]) }
    }

    #[inline]
    /// The given files are loaded sequentially, returning the index of the current file
    /// and the error in case of an error. [More information.](https://mpv.io/manual/master/#command-interface-[replace|append|append-play)
    ///
    /// # Arguments
    /// The tuple consists of:
    ///     * a string slice - the path
    ///     * a `FileState` - how the file will be opened
    ///     * an optional string slice - any additional options that will be set for this file
    ///
    /// # Peculiarities
    /// `loadfile` is kind of asynchronous, any additional option is set during loading, [specifics](https://github.com/mpv-player/mpv/issues/4089).
    fn playlist_load_files<'a, A>(&self, files: &[(&str, FileState, A)]) -> Result<()>
        where A: Into<Option<&'a str>> + Clone
    {
        for (i, elem) in files.iter().enumerate() {
            let args = elem.2.clone().into().unwrap_or("");

            let ret = unsafe {
                self.command("loadfile",
                             &[&format!("\"{}\"", elem.0), elem.1.val(), args])
            };

            if ret.is_err() {
                return Err(ErrorKind::Loadfiles(i, Box::new(ret.unwrap_err())).into());
            }
        }
        Ok(())
    }

    #[inline]
    /// Load the given playlist file, that either replaces the current playlist, or appends to it.
    fn playlist_load_list(&self, path: &str, replace: bool) -> Result<()> {
        if replace {
            unsafe { self.command("loadlist", &[&format!("\"{}\"", path), "replace"]) }
        } else {
            unsafe { self.command("loadlist", &[&format!("\"{}\"", path), "append"]) }
        }
    }

    #[inline]
    /// Remove every, except the current, item from the playlist.
    fn playlist_clear(&self) -> Result<()> {
        unsafe { self.command("playlist-clear", &[]) }
    }

    #[inline]
    /// Remove the currently selected item from the playlist.
    fn playlist_remove_current(&self) -> Result<()> {
        unsafe { self.command("playlist-remove", &["current"]) }
    }

    #[inline]
    /// Remove item at `position` from the playlist.
    fn playlist_remove_index(&self, position: usize) -> Result<()> {
        unsafe { self.command("playlist-remove", &[&format!("{}", position)]) }
    }

    #[inline]
    /// Move item `old` to the position of item `new`.
    fn playlist_move(&self, old: usize, new: usize) -> Result<()> {
        unsafe { self.command("playlist-move", &[&format!("{}", new), &format!("{}", old)]) }
    }

    #[inline]
    /// Shuffle the playlist.
    fn playlist_shuffle(&self) -> Result<()> {
        unsafe { self.command("playlist-shuffle", &[]) }
    }

    // --- Subtitle functions ---
    //

    #[inline]
    /// Add and select the subtitle immediately.
    /// Specifying a language requires specifying a title.
    fn subtitle_add_select<'a, 'b, A: Into<Option<&'a str>>, B: Into<Option<&'b str>>>
        (&self,
         path: &str,
         title: A,
         lang: B)
         -> Result<()> {
        match (title.into(), lang.into()) {
            (None, None) => unsafe {
                self.command("sub-add", &[&format!("\"{}\"", path), "select"])
            },
            (Some(t), None) => unsafe {
                self.command("sub-add", &[&format!("\"{}\"", path), "select", t])
            },
            (None, Some(_)) => Err(ErrorKind::InvalidArgument.into()),
            (Some(t), Some(l)) => unsafe {
                self.command("sub-add", &[&format!("\"{}\"", path), "select", t, l])
            },
        }
    }

    #[inline]
    /// See `AddSelect`. "Don't select the subtitle.
    /// (Or in some special situations, let the default stream selection mechanism decide.)".
    ///
    /// Returns an `Error::InvalidArgument` if a language, but not a title, was provided.
    fn subtitle_add_auto<'a, 'b, A: Into<Option<&'a str>>, B: Into<Option<&'b str>>>
        (&self,
         path: &str,
         title: A,
         lang: B)
         -> Result<()> {
        match (title.into(), lang.into()) {
            (None, None) => unsafe { self.command("sub-add", &[&format!("\"{}\"", path), "auto"]) },
            (Some(t), None) => unsafe {
                self.command("sub-add", &[&format!("\"{}\"", path), "auto", t])
            },
            (Some(t), Some(l)) => unsafe {
                self.command("sub-add", &[&format!("\"{}\"", path), "auto", t, l])
            },
            (None, Some(_)) => Err(ErrorKind::InvalidArgument.into()),
        }
    }

    #[inline]
    /// See `AddSelect`. "Select the subtitle. If a subtitle with the same file name was
    /// already added, that one is selected, instead of loading a duplicate entry.
    /// (In this case, title/language are ignored, and if the [sub] was changed since it was loaded,
    /// these changes won't be reflected.)".
    fn subtitle_add_cached(&self, path: &str) -> Result<()> {
        unsafe { self.command("sub-add", &[&format!("\"{}\"", path), "cached"]) }
    }

    #[inline]
    /// "Remove the given subtitle track. If the id argument is missing, remove the current
    /// track. (Works on external subtitle files only.)"
    fn subtitle_remove<A: Into<Option<usize>>>(&self, index: A) -> Result<()> {
        if let Some(idx) = index.into() {
            unsafe { self.command("sub-remove", &[&format!("{}", idx)]) }
        } else {
            unsafe { self.command("sub-remove", &[]) }
        }
    }

    #[inline]
    /// "Reload the given subtitle track. If the id argument is missing, reload the current
    /// track. (Works on external subtitle files only.)"
    fn subtitle_reload<A: Into<Option<usize>>>(&self, index: A) -> Result<()> {
        if let Some(idx) = index.into() {
            unsafe { self.command("sub-reload", &[&format!("{}", idx)]) }
        } else {
            unsafe { self.command("sub-reload", &[]) }
        }
    }

    #[inline]
    /// "Change subtitle timing such, that the subtitle event after the next `isize` subtitle
    /// events is displayed. `isize` can be negative to step backwards."
    fn subtitle_step(&self, skip: isize) -> Result<()> {
        unsafe { self.command("sub-step", &[&format!("{}", skip)]) }
    }

    #[inline]
    /// "Seek to the next subtitle. This is similar to sub-step, except that it seeks video and
    /// audio instead of adjusting the subtitle delay.
    /// For embedded subtitles (like with matroska), this works only with subtitle events that
    /// have already been displayed, or are within a short prefetch range."
    fn subtitle_seek_forward(&self) -> Result<()> {
        unsafe { self.command("sub-seek", &["1"]) }
    }

    #[inline]
    /// See `SeekForward`.
    fn subtitle_seek_backward(&self) -> Result<()> {
        unsafe { self.command("sub-seek", &["-1"]) }
    }
}

impl MpvInstance for Parent {
    fn ctx(&self) -> *mut MpvHandle {
        self.ctx
    }
    fn events(&self) -> bool {
        self.events
    }
    fn ev_iter_notification(&self) -> &Option<Box<(Mutex<bool>, Condvar)>> {
        &self.ev_iter_notification
    }
    fn ev_to_observe(&self) -> &Option<Mutex<Vec<Event>>> {
        &self.ev_to_observe
    }
    fn ev_to_observe_properties(&self) -> &Option<Mutex<HashMap<String, libc::uint64_t>>> {
        &self.ev_to_observe_properties
    }
    fn ev_observed(&self) -> &Option<Mutex<Vec<Event>>> {
        &self.ev_observed
    }
}

impl<'parent> MpvInstance for Client<'parent> {
    fn ctx(&self) -> *mut MpvHandle {
        self.ctx
    }
    fn events(&self) -> bool {
        self.events
    }
    fn ev_iter_notification(&self) -> &Option<Box<(Mutex<bool>, Condvar)>> {
        &self.ev_iter_notification
    }
    fn ev_to_observe(&self) -> &Option<Mutex<Vec<Event>>> {
        &self.ev_to_observe
    }
    fn ev_to_observe_properties(&self) -> &Option<Mutex<HashMap<String, libc::uint64_t>>> {
        &self.ev_to_observe_properties
    }
    fn ev_observed(&self) -> &Option<Mutex<Vec<Event>>> {
        &self.ev_observed
    }
}
