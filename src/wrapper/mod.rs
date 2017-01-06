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

#[cfg(feature="events")]
/// Contains event related abstractions
pub mod events;
/// Contains abstractions to define custom protocol handlers.
pub mod protocol;
/// Contains abstractions to use the opengl callback interface.
pub mod opengl_cb;

use enum_primitive::FromPrimitive;
use libc;
use parking_lot::{Condvar, Mutex, MutexGuard, Once, ONCE_INIT};

use super::raw::*;
#[cfg(feature="events")]
use events::*;
#[cfg(feature="events")]
use events::event_callback;
use protocol::*;
use opengl_cb::*;

use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::marker::PhantomData;
use std::mem;
use std::path::Path;
use std::ptr;
use std::rc::Rc;
use std::time::Duration;

#[cfg(unix)]
use std::ffi::OsStr;
#[cfg(unix)]
use std::os::unix::ffi::OsStrExt;

static SET_LC_NUMERIC: Once = ONCE_INIT;

// Cast `&mut Data` so that libmpv can use it.
macro_rules! data_ptr {
    ($data:expr) => (
        match $data {
            &mut Data::Flag(ref mut v) => v as *mut bool as *mut libc::c_void,
            &mut Data::Int64(ref mut v) => v as *mut libc::int64_t as *mut libc::c_void,
            &mut Data::Double(ref mut v) => v as *mut libc::c_double as *mut libc::c_void,
            _ => unreachable!(),
        }
    )
}

macro_rules! destroy_on_err {
    ($ctx:expr, $exec:expr) => (
        {
            let err = mpv_err((), $exec);
            if err.is_err() {
                mpv_terminate_destroy($ctx);
                return Err(err.unwrap_err());
            }
        }
    )
}

macro_rules! detach_on_err {
    ($ctx:expr, $exec:expr) => (
        {
            let err = mpv_err((), $exec);
            if err.is_err() {
                mpv_detach_destroy($ctx);
                return Err(err.unwrap_err());
            }
        }
    )
}

fn mpv_err<T>(ret: T, err_val: libc::c_int) -> Result<T, Error> {
    debug_assert!(err_val <= 0);
    if err_val == 0 {
        Ok(ret)
    } else {
        Err(Error::Mpv(MpvError::from_i32(err_val).unwrap()))
    }
}

#[cfg(unix)]
fn mpv_cstr_to_string(cstr: &CStr) -> String {
    OsStr::from_bytes(cstr.to_bytes()).to_string_lossy().into_owned()
}

#[cfg(windows)]
fn mpv_cstr_to_string(cstr: &CStr) -> String {
    // Mpv returns all strings on windows in UTF-8.
    cstr.to_str().unwrap().to_owned()
}

#[cfg(all(not(unix), not(windows)))]
fn mpv_cstr_to_string(cstr: &CStr) -> String {
    // Hope that all is well
    String::from_utf8_lossy(cstr.to_bytes()).into_owned()
}

#[derive(Clone, Debug, PartialEq)]
/// All possible error values returned by this crate.
pub enum Error {
    /// An API call did not execute successfully.
    Mpv(MpvError),
    /// All `suspend` calls have already been undone.
    AlreadyResumed,
    /// Some functions only accept absolute paths.
    ExpectedAbsolute,
    /// If a file was expected, but a directory was given.
    ExpectedFile,
    /// If a command failed during a `loadfiles` call, contains index of failed command and `Error`.
    Loadfiles((usize, Rc<Error>)),
    #[cfg(feature="events")]
    /// This event is already being observed by another `EventIter`.
    AlreadyObserved(Rc<Event>),
    /// Used a `Data::OsdString` while writing.
    OsdStringWrite,
    /// The library was compiled against a different mpv version than what is present on the system.
    /// First value is compiled version, second value is loaded version.
    VersionMismatch(u32, u32),
    /// An opengl callback has already been registered.
    CallbackExists,
    /// Mpv returned null while creating the core.
    Null,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(missing_docs)]
/// Data types that are used by the API.
pub enum Data {
    String(String),
    OsdString(String),
    Flag(bool),
    Int64(libc::int64_t),
    Double(libc::c_double),
}

impl Data {
    #[inline]
    /// Create a `Data`.
    pub fn new<T>(val: T) -> Data
        where T: Into<Data>
    {
        val.into()
    }

    fn format(&self) -> MpvFormat {
        match *self {
            Data::String(_) => MpvFormat::String,
            Data::OsdString(_) => MpvFormat::OsdString,
            Data::Flag(_) => MpvFormat::Flag,
            Data::Int64(_) => MpvFormat::Int64,
            Data::Double(_) => MpvFormat::Double,
        }
    }

    fn from_raw(fmt: MpvFormat, ptr: *mut libc::c_void) -> Data {
        debug_assert!(!ptr.is_null());
        match fmt {
            MpvFormat::Flag => Data::Flag(unsafe { *(ptr as *mut libc::int64_t) } != 0 ),
            MpvFormat::Int64 => Data::Int64(unsafe { *(ptr as *mut _) }),
            MpvFormat::Double => Data::Double(unsafe { *(ptr as *mut _) }),
            _ => unreachable!(),
        }
    }
}

impl Into<Data> for String {
    #[inline]
    fn into(self) -> Data {
        Data::String(self)
    }
}

impl<'a> Into<Data> for &'a str {
    #[inline]
    fn into(self) -> Data {
        Data::String(self.to_owned())
    }
}

impl Into<Data> for bool {
    #[inline]
    fn into(self) -> Data {
        Data::Flag(self)
    }
}

impl Into<Data> for i32 {
    #[inline]
    fn into(self) -> Data {
        Data::Int64(self as _)
    }
}

impl Into<Data> for i64 {
    #[inline]
    fn into(self) -> Data {
        Data::Int64(self as _)
    }
}

impl Into<Data> for u32 {
    #[inline]
    fn into(self) -> Data {
        Data::Int64(self as _)
    }
}

impl Into<Data> for f32 {
    #[inline]
    fn into(self) -> Data {
        Data::Double(self as _)
    }
}

impl Into<Data> for f64 {
    #[inline]
    fn into(self) -> Data {
        Data::Double(self as _)
    }
}

#[allow(missing_docs)]
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
pub struct Parent<T, U> {
    ctx: *mut MpvHandle,
    #[cfg(feature="events")]
    ev_iter_notification: *mut (Mutex<bool>, Condvar),
    #[cfg(feature="events")]
    ev_to_observe: Mutex<Vec<Event>>,
    #[cfg(feature="events")]
    ev_to_observe_properties: Mutex<HashMap<String, libc::uint64_t>>,
    #[cfg(feature="events")]
    ev_observed: Mutex<Vec<InnerEvent>>,
    custom_protocols: Mutex<Vec<Protocol<T, U>>>,
    opengl_state: Mutex<OpenGlState>,
}

/// A client of a `Parent`.
pub struct Client<'parent, T: 'parent, U: 'parent> {
    ctx: *mut MpvHandle,
    #[cfg(feature="events")]
    ev_iter_notification: *mut (Mutex<bool>, Condvar),
    #[cfg(feature="events")]
    ev_to_observe: Mutex<Vec<Event>>,
    #[cfg(feature="events")]
    ev_observed: Mutex<Vec<InnerEvent>>,
    #[cfg(feature="events")]
    ev_to_observe_properties: Mutex<HashMap<String, libc::uint64_t>>,
    _does_not_outlive: PhantomData<&'parent Parent<T, U>>,
}

unsafe impl<T, U> Send for Parent<T, U> {}
unsafe impl<T, U> Sync for Parent<T, U> {}
unsafe impl<'parent, T, U> Send for Client<'parent, T, U> {}
unsafe impl<'parent, T, U> Sync for Client<'parent, T, U> {}

#[doc(hidden)]
#[allow(missing_docs)]
/// Designed for internal use.
pub unsafe trait MpvMarker {
    // FIXME: Most of these can go once `Associated Fields` lands
    fn ctx(&self) -> *mut MpvHandle;
    #[cfg(feature="events")]
    fn ev_iter_notification(&self) -> *mut (Mutex<bool>, Condvar);
    #[cfg(feature="events")]
    fn ev_to_observe(&self) -> &Mutex<Vec<Event>>;
    #[cfg(feature="events")]
    fn ev_to_observe_properties(&self) -> &Mutex<HashMap<String, libc::uint64_t>>;
    #[cfg(feature="events")]
    fn ev_observed(&self) -> &Mutex<Vec<InnerEvent>>;
    #[cfg(feature="events")]
    unsafe fn drop_ev_iter(&mut self) {
        Box::from_raw(self.ev_iter_notification());
    }
}

unsafe impl<T, U> MpvMarker for Parent<T, U> {
    fn ctx(&self) -> *mut MpvHandle {
        self.ctx
    }
    #[cfg(feature="events")]
    fn ev_iter_notification(&self) -> *mut (Mutex<bool>, Condvar) {
        self.ev_iter_notification
    }
    #[cfg(feature="events")]
    fn ev_to_observe(&self) -> &Mutex<Vec<Event>> {
        &self.ev_to_observe
    }
    #[cfg(feature="events")]
    fn ev_to_observe_properties(&self) -> &Mutex<HashMap<String, libc::uint64_t>> {
        &self.ev_to_observe_properties
    }
    #[cfg(feature="events")]
    fn ev_observed(&self) -> &Mutex<Vec<InnerEvent>> {
        &self.ev_observed
    }
}

unsafe impl<'parent, T, U> MpvMarker for Client<'parent, T, U> {
    fn ctx(&self) -> *mut MpvHandle {
        self.ctx
    }
    #[cfg(feature="events")]
    fn ev_iter_notification(&self) -> *mut (Mutex<bool>, Condvar) {
        self.ev_iter_notification
    }
    #[cfg(feature="events")]
    fn ev_to_observe(&self) -> &Mutex<Vec<Event>> {
        &self.ev_to_observe
    }
    #[cfg(feature="events")]
    fn ev_to_observe_properties(&self) -> &Mutex<HashMap<String, libc::uint64_t>> {
        &self.ev_to_observe_properties
    }
    #[cfg(feature="events")]
    fn ev_observed(&self) -> &Mutex<Vec<InnerEvent>> {
        &self.ev_observed
    }
}

impl<T, U> Drop for Parent<T, U> {
    #[inline]
    fn drop(&mut self) {
        unsafe {
            #[cfg(feature="events")]
            self.drop_ev_iter();
            let opengl_ctx = self.opengl_state.lock().api_ctx;
            if !opengl_ctx.is_null() {
                mpv_opengl_cb_uninit_gl(opengl_ctx);
            }
            mpv_terminate_destroy(self.ctx());
        }
    }
}

impl<'parent, T, U> Drop for Client<'parent, T, U> {
    #[inline]
    fn drop(&mut self) {
        unsafe {
            #[cfg(feature="events")]
            self.drop_ev_iter();
            mpv_detach_destroy(self.ctx());
        }
    }
}

impl<T, U> Parent<T, U> {
    #[inline]
    /// Create a new `Parent`.
    /// The default settings can be probed by running: `$ mpv --show-profile=libmpv`
    pub fn new() -> Result<Parent<T, U>, Error> {
        Parent::internal_new::<&str>(&[])
    }

    #[inline]
    /// Create a new `Parent`, with the given settings set before initialization.
    pub fn with_options<A>(opts: &[(&str, &A)]) -> Result<Parent<T, U>, Error>
        where A: Into<Data> + Clone
    {
        Parent::internal_new(opts)
    }

    fn internal_new<A>(opts: &[(&str, &A)]) -> Result<Parent<T, U>, Error>
        where A: Into<Data> + Clone
    {
        SET_LC_NUMERIC.call_once(|| {
            let c = CString::new("C").unwrap();
            unsafe { libc::setlocale(libc::LC_NUMERIC, c.as_ptr()) };
        });

        let api_version = unsafe { mpv_client_api_version() };
        if super::MPV_CLIENT_API_VERSION != api_version {
            return Err(Error::VersionMismatch(super::MPV_CLIENT_API_VERSION, api_version));
        }

        let ctx = unsafe { mpv_create() };
        if ctx.is_null() {
            return Err(Error::Null);
        }

        unsafe {
            // Disable deprecated events.
            destroy_on_err!(ctx, mpv_request_event(ctx, MpvEventId::TracksChanged, 0));
            destroy_on_err!(ctx, mpv_request_event(ctx, MpvEventId::TrackSwitched, 0));
            destroy_on_err!(ctx, mpv_request_event(ctx, MpvEventId::Pause, 0));
            destroy_on_err!(ctx, mpv_request_event(ctx, MpvEventId::Unpause, 0));
            destroy_on_err!(ctx,
                            mpv_request_event(ctx, MpvEventId::ScriptInputDispatch, 0));
            destroy_on_err!(ctx, mpv_request_event(ctx, MpvEventId::MetadataUpdate, 0));
            destroy_on_err!(ctx, mpv_request_event(ctx, MpvEventId::ChapterChange, 0));
        }

        #[cfg(feature="events")]
        let (ev_iter_notification, ev_to_observe, ev_to_observe_properties, ev_observed) =
         {
                let ev_iter_notification = Box::into_raw(Box::new((Mutex::new(false),
                                                                   Condvar::new())));
                unsafe {
                    mpv_set_wakeup_callback(ctx,
                                            event_callback,
                                            ev_iter_notification as *mut _);
                }

                (ev_iter_notification,
                 Mutex::new(Vec::with_capacity(15)),
                 Mutex::new(HashMap::with_capacity(10)),
                 Mutex::new(Vec::with_capacity(22)))
            };
            #[cfg(not(feature="events"))]
             {
                unsafe {
                    // Disable remaining events
                    destroy_on_err!(ctx, mpv_request_event(ctx, MpvEventId::LogMessage, 0));
                    destroy_on_err!(ctx, mpv_request_event(ctx, MpvEventId::GetPropertyReply, 0));
                    destroy_on_err!(ctx, mpv_request_event(ctx, MpvEventId::SetPropertyReply, 0));
                    destroy_on_err!(ctx, mpv_request_event(ctx, MpvEventId::CommandReply, 0));
                    destroy_on_err!(ctx, mpv_request_event(ctx, MpvEventId::StartFile, 0));
                    destroy_on_err!(ctx, mpv_request_event(ctx, MpvEventId::EndFile, 0));
                    destroy_on_err!(ctx, mpv_request_event(ctx, MpvEventId::FileLoaded, 0));
                    destroy_on_err!(ctx, mpv_request_event(ctx, MpvEventId::Idle, 0));
                    destroy_on_err!(ctx, mpv_request_event(ctx, MpvEventId::ClientMessage, 0));
                    destroy_on_err!(ctx, mpv_request_event(ctx, MpvEventId::VideoReconfig, 0));
                    destroy_on_err!(ctx, mpv_request_event(ctx, MpvEventId::AudioReconfig, 0));
                    destroy_on_err!(ctx, mpv_request_event(ctx, MpvEventId::Seek, 0));
                    destroy_on_err!(ctx, mpv_request_event(ctx, MpvEventId::PlaybackRestart, 0));
                    destroy_on_err!(ctx, mpv_request_event(ctx, MpvEventId::PropertyChange, 0));
                    destroy_on_err!(ctx, mpv_request_event(ctx, MpvEventId::QueueOverflow, 0));
                }
            }

        for opt in opts {
            // At this point ret can only be Ok
            let err = internal_set_property(ctx, opt.0, opt.1.clone());
            if err.is_err() {
                unsafe {
                    destroy_on_err!(ctx, -20);    
                }
                return Err(err.unwrap_err());
            }
        }
        
        unsafe { destroy_on_err!(ctx, mpv_initialize(ctx)) }

        let ret;
        #[cfg(not(feature="events"))] {
            ret = Ok(Parent {
                ctx: ctx,
                custom_protocols: Mutex::new(Vec::new()),
                opengl_state: Mutex::new(OpenGlState::empty()),
            });
        }

        #[cfg(feature="events")] {
            ret = Ok(Parent {
                ctx: ctx,
                ev_iter_notification: ev_iter_notification,
                ev_to_observe: ev_to_observe,
                ev_to_observe_properties: ev_to_observe_properties,
                ev_observed: ev_observed,
                custom_protocols: Mutex::new(Vec::new()),
                opengl_state: Mutex::new(OpenGlState::empty()),
            });
        }
        ret
    }

    #[inline]
    /// Create a client with `name`, that is connected to the core of `self`, but has its own queue
    /// for API events and such.
    pub fn new_client(&self, name: &str) -> Result<Client<T, U>, Error> {
        let ctx = unsafe {
            let name = CString::new(name).unwrap();
            mpv_create_client(self.ctx(), name.as_ptr())
        };
        unsafe {
            // Disable deprecated events.
            detach_on_err!(ctx, mpv_request_event(ctx, MpvEventId::TracksChanged, 0));
            detach_on_err!(ctx, mpv_request_event(ctx, MpvEventId::TrackSwitched, 0));
            detach_on_err!(ctx, mpv_request_event(ctx, MpvEventId::Pause, 0));
            detach_on_err!(ctx, mpv_request_event(ctx, MpvEventId::Unpause, 0));
            detach_on_err!(ctx, mpv_request_event(ctx, MpvEventId::ScriptInputDispatch, 0));
            detach_on_err!(ctx, mpv_request_event(ctx, MpvEventId::MetadataUpdate, 0));
            detach_on_err!(ctx, mpv_request_event(ctx, MpvEventId::ChapterChange, 0));
        }
        #[cfg(feature="events")]
        let (ev_iter_notification, ev_to_observe, ev_to_observe_properties, ev_observed) =
            {
                let ev_iter_notification = Box::into_raw(Box::new((Mutex::new(false),
                                                                   Condvar::new())));
                unsafe {
                    mpv_set_wakeup_callback(ctx,
                                            event_callback,
                                            ev_iter_notification as *mut _);
                }

                (ev_iter_notification,
                 Mutex::new(Vec::with_capacity(15)),
                 Mutex::new(HashMap::with_capacity(10)),
                 Mutex::new(Vec::with_capacity(22)))
            };

            #[cfg(not(feature="events"))] {
                unsafe {
                    // Disable remaining events
                    detach_on_err!(ctx, mpv_request_event(ctx, MpvEventId::LogMessage, 0));
                    detach_on_err!(ctx, mpv_request_event(ctx, MpvEventId::GetPropertyReply, 0));
                    detach_on_err!(ctx, mpv_request_event(ctx, MpvEventId::SetPropertyReply, 0));
                    detach_on_err!(ctx, mpv_request_event(ctx, MpvEventId::CommandReply, 0));
                    detach_on_err!(ctx, mpv_request_event(ctx, MpvEventId::StartFile, 0));
                    detach_on_err!(ctx, mpv_request_event(ctx, MpvEventId::EndFile, 0));
                    detach_on_err!(ctx, mpv_request_event(ctx, MpvEventId::FileLoaded, 0));
                    detach_on_err!(ctx, mpv_request_event(ctx, MpvEventId::Idle, 0));
                    detach_on_err!(ctx, mpv_request_event(ctx, MpvEventId::ClientMessage, 0));
                    detach_on_err!(ctx, mpv_request_event(ctx, MpvEventId::VideoReconfig, 0));
                    detach_on_err!(ctx, mpv_request_event(ctx, MpvEventId::AudioReconfig, 0));
                    detach_on_err!(ctx, mpv_request_event(ctx, MpvEventId::Seek, 0));
                    detach_on_err!(ctx, mpv_request_event(ctx, MpvEventId::PlaybackRestart, 0));
                    detach_on_err!(ctx, mpv_request_event(ctx, MpvEventId::PropertyChange, 0));
                    detach_on_err!(ctx, mpv_request_event(ctx, MpvEventId::QueueOverflow, 0));
                }
            }

        let ret;
        #[cfg(not(feature="events"))]
        {
            ret = Ok(Client {
                ctx: ctx,
                _does_not_outlive: PhantomData::<&Self>,
            });
        }
        #[cfg(feature="events")]
        {
            ret = Ok(Client {
                ctx: ctx,
                ev_iter_notification: ev_iter_notification,
                ev_to_observe: ev_to_observe,
                ev_to_observe_properties: ev_to_observe_properties,
                ev_observed: ev_observed,
                _does_not_outlive: PhantomData::<&Self>,
            });
        }
        ret
    }

    #[inline]
    /// Enable opengl callback for this `Parent`.
    pub fn init_opengl_callback<F>(&self, procaddr: F) -> Result<MutexGuard<OpenGlState>, Error>
        where F: for<'a> Fn(&'a str) -> *const () + 'static
    {
        let guard = self.opengl_state.try_lock();

        if guard.is_none() {
            Err(Error::CallbackExists)
        } else {
            let mut guard = guard.unwrap();
            *guard = OpenGlState::new(self.ctx, procaddr)?;
            Ok(guard)
        }
    }

    #[inline]
    /// Register a custom `Protocol`. Once a protocol has been registered, it lives as long as the
    /// `Parent`.
    ///
    /// Returns `Error::Mpv(MpvError::InvalidParameter)` if a protocol with the same name has
    /// already been registered.
    pub fn register_protocol(&self, protocol: Protocol<T, U>) -> Result<(), Error> {
        let mut protocols = self.custom_protocols.lock();
        protocol.register(self.ctx)?;
        protocols.push(protocol);
        Ok(())
    }
}

impl<'parent, T, U> Client<'parent, T, U> {
    #[inline]
    /// Returns the name associated with `self`.
    pub fn name(&self) -> &str {
        unsafe { CStr::from_ptr(mpv_client_name(self.ctx())).to_str().unwrap() }
    }
}

#[allow(missing_docs)]
/// Core functionality that is supported by both `Client` and `Parent`.
/// See trait implementation for documentation.
pub trait MpvInstance<'parent, P>
    where P: MpvMarker + 'parent
{
    fn load_config(&self, path: &Path) -> Result<(), Error>;
    #[cfg(feature="events")]
    fn observe_events(&self, events: &[Event]) -> Result<EventIter<P>, Error>;
    unsafe fn command(&self, name: &str, args: &[&str]) -> Result<(), Error>;
    fn set_property<D: Into<Data>>(&self, name: &str, data: D) -> Result<(), Error>;
    fn get_property(&self, name: &str, format: Format) -> Result<Data, Error>;

    fn add_property(&self, property: &str, value: isize) -> Result<(), Error>;
    fn cycle_property(&self, property: &str, up: bool) -> Result<(), Error>;
    fn multiply_property(&self, property: &str, factor: usize) -> Result<(), Error>;
    fn pause(&self) -> Result<(), Error>;
    fn unpause(&self) -> Result<(), Error>;

    fn seek_forward(&self, time: &Duration) -> Result<(), Error>;
    fn seek_backward(&self, time: &Duration) -> Result<(), Error>;
    fn seek_absolute(&self, time: &Duration) -> Result<(), Error>;
    fn seek_percent(&self, percent: isize) -> Result<(), Error>;
    fn seek_percent_absolute(&self, percent: usize) -> Result<(), Error>;
    fn seek_revert(&self) -> Result<(), Error>;
    fn seek_revert_mark(&self) -> Result<(), Error>;
    fn seek_frame(&self) -> Result<(), Error>;
    fn seek_frame_backward(&self) -> Result<(), Error>;

    fn screenshot_subtitles<'a, A: Into<Option<&'a str>>>(&self, path: A) -> Result<(), Error>;
    fn screenshot_video<'a, A: Into<Option<&'a str>>>(&self, path: A) -> Result<(), Error>;
    fn screenshot_window<'a, A: Into<Option<&'a str>>>(&self, path: A) -> Result<(), Error>;

    fn playlist_next_weak(&self) -> Result<(), Error>;
    fn playlist_next_force(&self) -> Result<(), Error>;
    fn playlist_previous_weak(&self) -> Result<(), Error>;
    fn playlist_previous_force(&self) -> Result<(), Error>;
    fn playlist_load_files<'a, A>(&self, files: &[(&str, FileState, A)]) -> Result<(), (usize, Error)> where A: Into<Option<&'a str>> + Clone;
    fn playlist_load_list(&self, path: &str, replace: bool) -> Result<(), Error>;
    fn playlist_clear(&self) -> Result<(), Error>;
    fn playlist_remove_current(&self) -> Result<(), Error>;
    fn playlist_remove_index(&self, position: usize) -> Result<(), Error>;
    fn playlist_move(&self, old: usize, new: usize) -> Result<(), Error>;
    fn playlist_shuffle(&self) -> Result<(), Error>;

    fn subtitle_add_select<'a, 'b, A: Into<Option<&'a str>>, B: Into<Option<&'b str>>>(&self, path: &str, title: A, lang: B) -> Result<(), Error>;
    fn subtitle_add_auto<'a, 'b, A: Into<Option<&'a str>>, B: Into<Option<&'b str>>>(&self, path: &str, title: A, lang: B) -> Result<(), Error>;
    fn subtitle_add_cached(&self, path: &str) -> Result<(), Error>;
    fn subtitle_remove<A: Into<Option<usize>>>(&self, index: A) -> Result<(), Error>;
    fn subtitle_reload<A: Into<Option<usize>>>(&self, index: A) -> Result<(), Error>;
    fn subtitle_step(&self, skip: isize) -> Result<(), Error>;
    fn subtitle_seek_forward(&self) -> Result<(), Error>;
    fn subtitle_seek_backward(&self) -> Result<(), Error>;
}

impl<'parent, P> MpvInstance<'parent, P> for P
    where P: MpvMarker + 'parent
{
    #[inline]
    /// Load a configuration file. The path has to be absolute, and a file.
    fn load_config(&self, path: &Path) -> Result<(), Error> {
        if path.is_relative() {
            Err(Error::ExpectedAbsolute)
        } else if !path.is_file() {
            Err(Error::ExpectedFile)
        } else {
            let file = CString::new(path.to_str().unwrap()).unwrap().into_raw();
            let ret = mpv_err((), unsafe { mpv_load_config_file(self.ctx(), file) });
            unsafe { CString::from_raw(file) };
            ret
        }
    }

    #[inline]
    #[cfg(feature="events")]
    /// Observe given `Event`s via an `EventIter`.
    fn observe_events(&self, events: &[Event]) -> Result<EventIter<P>, Error> {
            let mut observe = self.ev_to_observe().lock();
            let mut properties = self.ev_to_observe_properties().lock();

            let len = events.len();
            let mut ids = Vec::with_capacity(len);
            let mut evs = Vec::with_capacity(len);
            let mut props = Vec::with_capacity(len);
            for elem in events {
                if let Event::PropertyChange(ref v) = *elem {
                    if properties.contains_key(&v.0) {
                        return Err(Error::AlreadyObserved(Rc::new(elem.clone())));
                    } else {
                        mpv_err((), unsafe { mpv_request_event(self.ctx(), elem.as_id(), 1) })?;
                        props.push(v);
                        ids.push(elem.as_id());
                        evs.push(elem.clone());
                    }
                } else {
                    for id in &*observe {
                        if elem.as_id() == id.as_id() {
                            return Err(Error::AlreadyObserved(Rc::new(elem.clone())));
                        }
                    }

                    if let Event::LogMessage(ref v) = *elem {
                        let min_level = CString::new(v.log_level.as_string()).unwrap();
                        mpv_err((), unsafe {
                            mpv_request_log_messages(self.ctx(), min_level.as_ptr())
                        })?;
                    }

                    mpv_err((), unsafe { mpv_request_event(self.ctx(), elem.as_id(), 1) })?;
                    ids.push(elem.as_id());
                    evs.push(elem.clone());
                }
            }

            let mut props_ins = Vec::with_capacity(len);
            let start_id = properties.len();
            for (i, elem) in props.iter().enumerate() {
                let name = CString::new(&elem.0[..]).unwrap();
                let err = mpv_err((),
                                  unsafe {
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
                notification: self.ev_iter_notification(),
                all_to_observe: self.ev_to_observe(),
                all_to_observe_properties: self.ev_to_observe_properties(),
                local_to_observe: evs,
                all_observed: self.ev_observed(),
                _does_not_outlive: PhantomData::<&Self>,
            })
    }

    #[inline]
    /// Send a command to the `Mpv` instance. This uses `mpv_command_string` internally,
    /// so that the syntax is the same as described in the [manual for the input.conf]
    /// (https://mpv.io/manual/master/#list-of-input-commands).
    ///
    /// # Safety
    /// This method is unsafe because arbitrary code may be executed resulting in UB and more.
    unsafe fn command(&self, name: &str, args: &[&str]) -> Result<(), Error> {
        let mut cmd = String::with_capacity(name.len() + args.iter()
                                                             .fold(0, |acc, e| acc + e.len() + 1));
        cmd.push_str(name);

        for elem in args {
            cmd.push_str(" ");
            cmd.push_str(elem);
        }
        let raw = CString::new(cmd).unwrap();

        mpv_err((), mpv_command_string(self.ctx(), raw.as_ptr()))
    }

    #[inline]
    /// Set the value of a property.
    fn set_property<T: Into<Data>>(&self, name: &str, data: T) -> Result<(), Error> {
        internal_set_property(self.ctx(), name, data)
    }

    #[inline]
    /// Get the value of a property.
    fn get_property(&self, name: &str, format: Format) -> Result<Data, Error> {
        let name = CString::new(name).unwrap();
        match format {
            Format::String | Format::OsdString => {
                let mut ptr = &mut ptr::null();

                let err = mpv_err((), unsafe {
                    mpv_get_property(self.ctx(),
                                     name.as_ptr(),
                                     format.as_mpv_format().as_val(),
                                     ptr as *mut *const libc::c_char as *mut _)
                });
                debug_assert!(!ptr.is_null());

                err.or_else(Err)
                    .and_then(|_| {
                        let ret = unsafe { CStr::from_ptr(*ptr) };

                        let data = mpv_cstr_to_string(ret);

                        unsafe{mpv_free(*ptr as *mut _)}

                        Ok(match format {
                            Format::String => Data::String(data),
                            Format::OsdString => Data::OsdString(data),
                            _ => unreachable!(),
                        })
                    })
            }
            _ => {
                let ptr = unsafe { &mut mem::zeroed() } as *mut Data as _;

                mpv_err((), unsafe {
                    mpv_get_property(self.ctx(),
                                     name.as_ptr(),
                                     format.as_mpv_format().as_val(),
                                     ptr)
                }).or_else(Err)
                  .and_then(|_| Ok(Data::from_raw(format.as_mpv_format(), ptr)))
            }
        }
    }

    // --- Convenience property functions ---
    //
    

    #[inline]
    /// Add -or subtract- any value from a property. Over/underflow clamps to max/min.
    fn add_property(&self, property: &str, value: isize) -> Result<(), Error> {
        unsafe { self.command("add", &[property, &format!("{}", value)]) }
    }

    #[inline]
    /// Cycle through a given property. `up` specifies direction. On
    /// overflow, set the property back to the minimum, on underflow set it to the maximum.
    fn cycle_property(&self, property: &str, up: bool) -> Result<(), Error> {
        unsafe {
            self.command("cycle",
                                       &[property,
                                         if up {
                                             "up"
                                         } else {
                                             "down"
                                         }])
        }
    }

    #[inline]
    /// Multiply any property with any positive factor.
    fn multiply_property(&self, property: &str, factor: usize) -> Result<(), Error> {
        unsafe {
            self.command("multiply", &[property, &format!("{}", factor)])
        }
    }

    #[inline]
    /// Pause playback at runtime.
    fn pause(&self) -> Result<(), Error> {
        self.set_property("pause", true)
    }

    #[inline]
    /// Unpause playback at runtime.
    fn unpause(&self) -> Result<(), Error> {
        self.set_property("pause", false)
    }

    // --- Convenience command functions ---
    //

    #[inline]
    /// Seek forward relatively from current position at runtime.
    /// This is less exact than `seek_absolute`, see [mpv manual]
    /// (https://mpv.io/manual/master/#command-interface-
    /// [relative|absolute|absolute-percent|relative-percent|exact|keyframes]).
    fn seek_forward(&self, time: &Duration) -> Result<(), Error> {
        unsafe {
            self.command("seek", &[&format!("{}", time.as_secs()), "relative"])
        }
    }

    #[inline]
    /// See `seek_forward`.
    fn seek_backward(&self, time: &Duration) -> Result<(), Error> {
        unsafe {
            self.command("seek", &[&format!("-{}", time.as_secs()), "relative"])
        }
    }

    #[inline]
    /// Seek to a given absolute time.
    fn seek_absolute(&self, time: &Duration) -> Result<(), Error> {
        unsafe {
            self.command("seek", &[&format!("{}", time.as_secs()), "absolute"])
        }
    }

    #[inline]
    /// Seek to a given relative percent position (may be negative).
    /// If `percent` of the playtime is bigger than the remaining playtime, the next file is played.
    /// out of bounds values are clamped to either 0 or 100.
    fn seek_percent(&self, percent: isize) -> Result<(), Error> {
        unsafe {
            self.command("seek", &[&format!("{}", percent), "relative-percent"])
        }
    }

    #[inline]
    /// Seek to the given percentage of the playtime.
    fn seek_percent_absolute(&self, percent: usize) -> Result<(), Error> {
        unsafe {
            self.command("seek", &[&format!("{}", percent), "relative-percent"])
        }
    }

    #[inline]
    /// Revert the previous `seek_` call, can also revert itself.
    fn seek_revert(&self) -> Result<(), Error> {
        unsafe { self.command("revert-seek", &[]) }
    }

    #[inline]
    /// Mark the current position as the position that will be seeked to by `seek_revert`.
    fn seek_revert_mark(&self) -> Result<(), Error> {
        unsafe { self.command("revert-seek", &["mark"]) }
    }

    #[inline]
    /// Seek exactly one frame, and pause.
    /// Noop on audio only streams.
    fn seek_frame(&self) -> Result<(), Error> {
        unsafe { self.command("frame-step", &[]) }
    }

    #[inline]
    /// See `seek_frame`.
    /// [Note performance considerations.](https://mpv.io/manual/master/#command-interface-frame-back-step)
    fn seek_frame_backward(&self) -> Result<(), Error> {
        unsafe { self.command("frame-back-step", &[]) }
    }

    #[inline]
    /// "Save the video image, in its original resolution, and with subtitles.
    /// Some video outputs may still include the OSD in the output under certain circumstances.".
    ///
    /// "[O]ptionally save it to a given file. The format of the file will be
    /// guessed by the extension (and --screenshot-format is ignored - the behaviour when the
    /// extension is missing or unknown is arbitrary). If the file already exists, it's overwritten.
    /// Like all input command parameters, the filename is subject to property expansion as
    /// described in [Property Expansion](https://mpv.io/manual/master/#property-expansion)."
    fn screenshot_subtitles<'a, A: Into<Option<&'a str>>>(&self, path: A) -> Result<(), Error> {
        let path = path.into();
        if path.is_none() {
            unsafe { self.command("screenshot", &["subtitles"]) }
        } else {
            unsafe { self.command("screenshot", &[path.unwrap(), "subtitles"]) }
        }        
    }

    #[inline]
    /// "Like subtitles, but typically without OSD or subtitles. The exact behavior depends on the selected
    /// video output."
    fn screenshot_video<'a, A: Into<Option<&'a str>>>(&self, path: A) -> Result<(), Error> {
        let path = path.into();
        if path.is_none() {
            unsafe { self.command("screenshot", &["video"]) }
        } else {
            unsafe { self.command("screenshot", &[path.unwrap(), "video"]) }
        }  
    }

    #[inline]
    /// "Save the contents of the mpv window. Typically scaled, with OSD and subtitles. The exact
    /// behaviour depends on the selected video output, and if no support is available,
    /// this will act like video.".
    fn screenshot_window<'a, A: Into<Option<&'a str>>>(&self, path: A) -> Result<(), Error> {
        let path = path.into();
        if path.is_none() {
            unsafe { self.command("screenshot", &["window"]) }
        } else {
            unsafe { self.command("screenshot", &[path.unwrap(), "window"]) }
        }  
    }

    #[inline]
    /// Play the next item of the current playlist.
    /// Does nothing if the current item is the last item.
    fn playlist_next_weak(&self) -> Result<(), Error> {
        unsafe {
            self.command("playlist-next", &["weak"])
        }
    }

    #[inline]
    /// Play the next item of the current playlist.
    /// Terminates playback if the current item is the last item.
    fn playlist_next_force(&self) -> Result<(), Error> {
        unsafe {
            self.command("playlist-next", &["force"])
        }
    }

    #[inline]
    /// See `playlist_next_weak`.
    fn playlist_previous_weak(&self) -> Result<(), Error> {
        unsafe {
            self.command("playlist-previous", &["weak"])
        }
    }

    #[inline]
    /// See `playlist_next_force`.
    fn playlist_previous_force(&self) -> Result<(), Error> {
        unsafe {
            self.command("playlist-previous", &["force"])
        }
    }

    #[inline]
    /// The given files are loaded sequentially, returning the index of the current file
    /// and the error in case of an error. [More information.](https://mpv.io/manual/master/#command-interface-[replace|append|append-play)
    fn playlist_load_files<'a, A>(&self, files: &[(&str, FileState, A)])
        -> Result<(), (usize, Error)> where A: Into<Option<&'a str>> + Clone
    {
        for (i, elem) in files.iter().enumerate() {
            let opts = elem.2.clone().into();
            let args = if opts.is_some() {
                opts.unwrap()
            } else {
                ""
            };

            let ret = unsafe {
                self.command("loadfile", &[elem.0, elem.1.val(), args])
            };

            if ret.is_err() {
                return Err((i, ret.unwrap_err()))
            }
        }
        Ok(())
    }

    #[inline]
    /// Load the given playlist file, that either replaces the current playlist, or appends to it.
    fn playlist_load_list(&self, path: &str, replace: bool) -> Result<(), Error> {
        if replace {
            unsafe {
                self.command("loadlist", &[&format!("\"{}\"", path), "replace"])
            }
        } else {
            unsafe {
                self.command("loadlist", &[&format!("\"{}\"", path), "append"])
            }
        }
    }

    #[inline]
    /// Remove every, except the current, item from the playlist.
    fn playlist_clear(&self) -> Result<(), Error> {
        unsafe {
            self.command("playlist-clear", &[])
        }
    }

    #[inline]
    /// Remove the currently selected item from the playlist.
    fn playlist_remove_current(&self) -> Result<(), Error> {
        unsafe {
            self.command("playlist-remove", &["current"])
        }
    }

    #[inline]
    /// Remove item at `position` from the playlist.
    fn playlist_remove_index(&self, position: usize) -> Result<(), Error> {
        unsafe {
            self.command("playlist-remove", &[&format!("{}", position)])
        }
    }

    #[inline]
    /// Move item `old` to the position of item `new`.
    fn playlist_move(&self, old: usize, new: usize) -> Result<(), Error> {
        unsafe {
            self.command("playlist-move", &[&format!("{}", new), &format!("{}", old)])
        }
    }

    #[inline]
    /// Shuffle the playlist.
    fn playlist_shuffle(&self) -> Result<(), Error> {
        unsafe {
            self.command("playlist-shuffle", &[])
        }
    }

    #[inline]
    /// Add and select the subtitle immediately.
    fn subtitle_add_select<'a, 'b, A: Into<Option<&'a str>>, B: Into<Option<&'b str>>>(&self, path: &str, title: A, lang: B)
         -> Result<(), Error>
    {
        match (title.into(), lang.into()) {
            (None, None) => {
                unsafe {
                    self.command("sub-add", &[path, "select"])
                }
            }
            (Some(t), None) => {
                unsafe {
                    self.command("sub-add", &[path, "select", t])
                }
            }
            (None, Some(l)) => {
                // TODO: This version is probably not supported (lang depends on title) -> throw err
                unsafe {
                    self.command("sub-add", &[path, "select", "", l])
                }   
            }
            (Some(t), Some(l)) => {
                unsafe {
                    self.command("sub-add", &[path, "select", t, l])
                }   
            }
        }
    }

    #[inline]
    /// See `AddSelect`. "Don't select the subtitle.
    /// (Or in some special situations, let the default stream selection mechanism decide.)".
    fn subtitle_add_auto<'a, 'b, A: Into<Option<&'a str>>, B: Into<Option<&'b str>>>(&self, path: &str, title: A, lang: B)
        -> Result<(), Error>
    {
        match (title.into(), lang.into()) {
            (None, None) => {
                unsafe {
                    self.command("sub-add", &[path, "auto"])
                }
            }
            (Some(t), None) => {
                unsafe {
                    self.command("sub-add", &[path, "auto", t])
                }
            }
            (None, Some(l)) => {
                unsafe {
                    self.command("sub-add", &[path, "auto", "", l])
                }   
            }
            (Some(t), Some(l)) => {
                unsafe {
                    self.command("sub-add", &[path, "auto", t, l])
                }   
            }
        }
    }

    #[inline]
    /// See `AddSelect`. "Select the subtitle. If a subtitle with the same file name was
    /// already added, that one is selected, instead of loading a duplicate entry.
    /// (In this case, title/language are ignored, and if the [sub] was changed since it was loaded,
    /// these changes won't be reflected.)".
    fn subtitle_add_cached(&self, path: &str) -> Result<(), Error> {
        unsafe {
            self.command("sub-add", &[path, "cached"])
        }
    }

    #[inline]
    /// "Remove the given subtitle track. If the id argument is missing, remove the current
    /// track. (Works on external subtitle files only.)"
    fn subtitle_remove<A: Into<Option<usize>>>(&self, index: A) -> Result<(), Error> {
        let index = index.into();
        if index.is_some() {
            unsafe {
                self.command("sub-remove", &[&format!("{}", index.unwrap())])
            }
        } else {
            unsafe {
                self.command("sub-remove", &[])
            }
        }
    }

    #[inline]
    /// "Reload the given subtitle track. If the id argument is missing, reload the current
    /// track. (Works on external subtitle files only.)"
    fn subtitle_reload<A: Into<Option<usize>>>(&self, index: A) -> Result<(), Error> {
        let index = index.into();
        if index.is_some() {
            unsafe {
                self.command("sub-reload", &[&format!("{}", index.unwrap())])
            }
        } else {
            unsafe {
                self.command("sub-reload", &[])
            }
        }
    }

    #[inline]
    /// "Change subtitle timing such, that the subtitle event after the next `isize` subtitle
    /// events is displayed. `isize` can be negative to step backwards."
    fn subtitle_step(&self, skip: isize) -> Result<(), Error> {
        unsafe {
            self.command("sub-step", &[&format!("{}", skip)])
        }
    }

    #[inline]
    /// "Seek to the next subtitle. This is similar to sub-step, except that it seeks video and
    /// audio instead of adjusting the subtitle delay.
    /// For embedded subtitles (like with matroska), this works only with subtitle events that
    /// have already been displayed, or are within a short prefetch range."
    fn subtitle_seek_forward(&self) -> Result<(), Error> {
        unsafe {
            self.command("sub-seek", &["1"])
        }
    }

    #[inline]
    /// See `SeekForward`.
    fn subtitle_seek_backward(&self) -> Result<(), Error> {
        unsafe {
            self.command("sub-seek", &["-1"])
        }
    }
}

fn internal_set_property<T: Into<Data>>(ctx: *mut MpvHandle, name: &str, data: T) 
    -> Result<(), Error>
{
    let name = CString::new(name).unwrap().into_raw();
    let mut data = data.into();
    let format = data.format().as_val();
    let ret = match data {
        Data::OsdString(_) => Err(Error::OsdStringWrite),
        Data::String(ref v) => {
            let data = CString::new(v.as_bytes()).unwrap();
            let ptr: *mut _ = &mut data.as_ptr();

            mpv_err((), unsafe { mpv_set_property(ctx, name, format, ptr as *mut _) })
        }
        _ => {
            let data = data_ptr!(&mut data);

            mpv_err((), unsafe { mpv_set_property(ctx, name, format, data) })
        }
    };
    unsafe { CString::from_raw(name) };
    ret
}