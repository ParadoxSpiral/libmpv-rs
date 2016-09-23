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

// FIXME: Clean the abstraction up: mod.rs, events.rs, utils.rs

/// Contains event related abstractions
pub mod events;
/// Contains miscellaneous abstractions
#[macro_use]
pub mod utils;
/// Contains abstractions to define custom protocol handlers.
pub mod protocol;

use libc;
use parking_lot::{Condvar, Mutex, Once, ONCE_INIT};

use super::raw::*;
use events::*;
use events::event_callback;
use protocol::*;
use utils::*;
use utils::mpv_err;

use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::marker::PhantomData;
use std::mem;
use std::path::Path;
use std::ptr;
use std::rc::Rc;
use std::time::Duration;

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

static SET_LC_NUMERIC: Once = ONCE_INIT;

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
    /// If an argument (like a percentage > 100) was out of bounds.
    OutOfBounds,
    /// If a command failed during a `loadfiles` call, contains index of failed command and `Error`.
    Loadfiles((usize, Rc<Error>)),
    /// Events are not enabled for this mpv instance.
    EventsDisabled,
    /// This event is already being observed by another `EventIter`.
    AlreadyObserved(Rc<Event>),
    /// Used a `Data::OsdString` while writing.
    OsdStringWrite,
    /// The library was compiled against a different mpv version than what is present on the system.
    /// First value is compiled version, second value is loaded version.
    VersionMismatch(u32, u32),
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
/// Seek operations supported by `seek`.
pub enum Seek {
    /// Seek forward relatively from current position at runtime.
    /// This is less exact than `seek_abs`, see [mpv manual]
    /// (https://mpv.io/manual/master/#command-interface-
    /// [relative|absolute|absolute-percent|relative-percent|exact|keyframes]).
    RelativeForward(Duration),
    /// See `RelativeForward`.
    RelativeBackward(Duration),
    /// Seek to a given absolute time at runtime.
    Absolute(Duration),
    /// Seek to a given relative percent position at runtime.
    /// If `usize` is bigger than the remaining playtime, the next file is played.
    RelativePercent(usize),
    /// Seek to a given absolute percent position at runtime.
    AbsolutePercent(usize),
    /// Revert one previous `seek` invocation. If this is called twice, this
    /// reverts the previous revert seek.
    Revert,
    /// Mark the current position. The next `seek_revert` call will revert
    /// to the marked position.
    RevertMark,
    /// Play exactly one frame, and then pause. This does nothing with
    /// audio-only playback.
    Frame,
    /// Play exactly the last frame, and then pause. This does nothing with
    /// audio-only playback. See [this]
    /// (https://mpv.io/manual/master/#command-interface-frame-back-step)
    /// for performance issues.
    FrameBack,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
/// Screenshot operations supported by `screenshot`.
pub enum Screenshot<'a> {
    /// "Save the video image, in its original resolution, and with subtitles.
    /// Some video outputs may still include the OSD in the output under certain circumstances.".
    Subtitles,
    /// "Take a screenshot and save it to a given file. The format of the file will be guessed by
    /// the extension (and --screenshot-format is ignored - the behaviour when the extension is
    /// missing or unknown is arbitrary). If the file already exists, it's overwritten. Like all
    /// input command parameters, the filename is subject to property expansion as described in
    /// Property Expansion.".
    SubtitlesFile(&'a Path),
    /// "Like subtitles, but typically without OSD or subtitles.
    /// The exact behaviour depends on the selected video output.".
    Video,
    /// See `Video`, with the addition of specifying a path.
    VideoFile(&'a Path),
    /// "Save the contents of the mpv window. Typically scaled, with OSD
    /// and subtitles. The exact behaviour depends on the selected video output, and if no support
    /// is available, this will act like video.".
    Window,
    /// See `Window`, with the addition of specifying a path.
    WindowFile(&'a Path),
}

#[derive(Clone, Copy, Debug)]
/// Playlist operations supported by `playlist`.
pub enum PlaylistOp<'a> {
    /// Play the next item of the current playlist.
    /// This does nothing if the current item is the last item.
    NextWeak,
    /// Play the next item of the current playlist.
    /// This terminates playback if the current item is the last item.
    NextForce,
    /// Play the previous item of the current playlist.
    /// This does nothing if the current item is the first item.
    PreviousWeak,
    /// Play the next item of the current playlist.
    /// This terminates playback if the current item is the first item.
    PreviousForce,
    /// Load any number of files with any playlist insertion behaviour,
    /// and any optional options that are set during playback of the specific item.
    Loadfiles(&'a [File<'a>]),
    /// Load the given playlist file. Replace current playlist.
    LoadlistReplace(&'a Path),
    /// Load the given playlist file. Append to current playlist.
    LoadlistAppend(&'a Path),
    /// Clear the current playlist, except the currently played item.
    Clear,
    /// Remove the currently selected playlist item.
    RemoveCurrent,
    /// Remove the item at position `usize`.
    RemoveIndex(usize),
    /// Move item `usize` to the position of item `usize`.
    Move((usize, usize)),
    /// Shuffle the playlist.
    Shuffle,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
/// Operations supported by `subtitle`.
pub enum SubOp<'a> {
    /// Add and select the subtitle immediately.
    /// The second argument is the title, third is the language.
    AddSelect(&'a Path, Option<&'a str>, Option<&'a str>),
    ///  See `AddSelect`. "Don't select the subtitle.
    /// (Or in some special situations, let the default stream selection mechanism decide.)".
    AddAuto(&'a Path, Option<&'a str>, Option<&'a str>),
    /// See `AddSelect`. "Select the subtitle. If a subtitle with the same file name was
    /// already added, that one is selected, instead of loading a duplicate entry.
    /// (In this case, title/language are ignored, and if the was changed since it was loaded,
    /// these changes won't be reflected.)".
    AddCached(&'a Path, Option<&'a str>, Option<&'a str>),
    /// Remove the given subtitle track. If the id argument is missing, remove the current
    /// track. (Works on external subtitle files only.)
    Remove(Option<usize>),
    /// Reload the given subtitle tracks. If the id argument is missing, reload the current
    /// track. (Works on external subtitle files only.)
    Reload(Option<usize>),
    /// Change subtitle timing such, that the subtitle event after the next `isize` subtitle
    /// events is displayed. `isize` can be negative to step backwards.
    Step(isize),
    /// Seek to the next subtitle. This is similar to sub-step, except that it seeks video and
    /// audio instead of adjusting the subtitle delay.
    /// For embedded subtitles (like with matroska), this works only with subtitle events that
    /// have already been displayed, or are within a short prefetch range.
    SeekForward,
    /// See `SeekForward`.
    SeekBackward,
}

/// An mpv instance from which `Client`s can be spawned.
pub struct Parent<T, U> {
    ctx: *mut MpvHandle,
    suspension_count: Mutex<usize>,
    check_events: bool,
    ev_iter_notification: Option<*mut (Mutex<bool>, Condvar)>,
    ev_to_observe: Option<Mutex<Vec<Event>>>,
    ev_to_observe_properties: Option<Mutex<HashMap<String, libc::uint64_t>>>,
    ev_observed: Option<Mutex<Vec<InnerEvent>>>,
    custom_protocols: Mutex<Vec<Protocol<T, U>>>,
}

/// A client of a `Parent`.
pub struct Client<'parent, T: 'parent, U: 'parent> {
    ctx: *mut MpvHandle,
    check_events: bool,
    ev_iter_notification: Option<*mut (Mutex<bool>, Condvar)>,
    ev_to_observe: Option<Mutex<Vec<Event>>>,
    ev_observed: Option<Mutex<Vec<InnerEvent>>>,
    ev_to_observe_properties: Option<Mutex<HashMap<String, libc::uint64_t>>>,
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
    // FIXME: Most of these can go once `Associated Items` lands
    fn ctx(&self) -> *mut MpvHandle;
    fn check_events(&self) -> bool;
    fn ev_iter_notification(&self) -> &Option<*mut (Mutex<bool>, Condvar)>;
    fn ev_to_observe(&self) -> &Option<Mutex<Vec<Event>>>;
    fn ev_to_observe_properties(&self) -> &Option<Mutex<HashMap<String, libc::uint64_t>>>;
    fn ev_observed(&self) -> &Option<Mutex<Vec<InnerEvent>>>;
    unsafe fn drop_ev_iter_step(&mut self) {
        if self.check_events() {
            Box::from_raw(self.ev_iter_notification().unwrap());
        }
    }
}

unsafe impl<T, U> MpvMarker for Parent<T, U> {
    fn ctx(&self) -> *mut MpvHandle {
        self.ctx
    }
    fn check_events(&self) -> bool {
        self.check_events
    }
    fn ev_iter_notification(&self) -> &Option<*mut (Mutex<bool>, Condvar)> {
        &self.ev_iter_notification
    }
    fn ev_to_observe(&self) -> &Option<Mutex<Vec<Event>>> {
        &self.ev_to_observe
    }
    fn ev_to_observe_properties(&self) -> &Option<Mutex<HashMap<String, libc::uint64_t>>> {
        &self.ev_to_observe_properties
    }
    fn ev_observed(&self) -> &Option<Mutex<Vec<InnerEvent>>> {
        &self.ev_observed
    }
}

unsafe impl<'parent, T, U> MpvMarker for Client<'parent, T, U> {
    fn ctx(&self) -> *mut MpvHandle {
        self.ctx
    }
    fn check_events(&self) -> bool {
        self.check_events
    }
    fn ev_iter_notification(&self) -> &Option<*mut (Mutex<bool>, Condvar)> {
        &self.ev_iter_notification
    }
    fn ev_to_observe(&self) -> &Option<Mutex<Vec<Event>>> {
        &self.ev_to_observe
    }
    fn ev_to_observe_properties(&self) -> &Option<Mutex<HashMap<String, libc::uint64_t>>> {
        &self.ev_to_observe_properties
    }
    fn ev_observed(&self) -> &Option<Mutex<Vec<InnerEvent>>> {
        &self.ev_observed
    }
}

impl<T, U> Drop for Parent<T, U> {
    #[inline]
    fn drop(&mut self) {
        unsafe {
            self.drop_ev_iter_step();
            mpv_terminate_destroy(self.ctx());
        }
    }
}

impl<'parent, T, U> Drop for Client<'parent, T, U> {
    #[inline]
    fn drop(&mut self) {
        unsafe {
            self.drop_ev_iter_step();
            mpv_detach_destroy(self.ctx());
        }
    }
}

impl<'parent, T, U> Parent<T, U> {
    #[inline]
    /// Create a new `Parent`.
    /// The default settings can be probed by running: `$ mpv --show-profile=libmpv`
    pub fn new(check_events: bool) -> Result<Parent<T, U>, Error> {
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

        let (ev_iter_notification, ev_to_observe, ev_to_observe_properties, ev_observed) =
            if check_events {
                let ev_iter_notification = Box::into_raw(Box::new((Mutex::new(false),
                                                                   Condvar::new())));
                unsafe {
                    mpv_set_wakeup_callback(ctx,
                                            event_callback,
                                            ev_iter_notification as *mut _);
                }

                (Some(ev_iter_notification),
                 Some(Mutex::new(Vec::with_capacity(15))),
                 Some(Mutex::new(HashMap::with_capacity(10))),
                 Some(Mutex::new(Vec::with_capacity(22))))
            } else {
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

                (None, None, None, None)
            };

        unsafe { destroy_on_err!(ctx, mpv_initialize(ctx)) }

        Ok(Parent {
            ctx: ctx,
            suspension_count: Mutex::new(0),
            check_events: check_events,
            ev_iter_notification: ev_iter_notification,
            ev_to_observe: ev_to_observe,
            ev_to_observe_properties: ev_to_observe_properties,
            ev_observed: ev_observed,
            custom_protocols: Mutex::new(Vec::new()),
        })
    }

    #[inline]
    /// Create a client with `name`, that is connected to the core of `self`, but has an own queue
    /// for API events and such.
    pub fn new_client(&self, name: &str, check_events: bool) -> Result<Client<T, U>, Error> {
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
            detach_on_err!(ctx,
                           mpv_request_event(ctx, MpvEventId::ScriptInputDispatch, 0));
            detach_on_err!(ctx, mpv_request_event(ctx, MpvEventId::MetadataUpdate, 0));
            detach_on_err!(ctx, mpv_request_event(ctx, MpvEventId::ChapterChange, 0));
        }
        let (ev_iter_notification, ev_to_observe, ev_to_observe_properties, ev_observed) =
            if check_events {
                let ev_iter_notification = Box::into_raw(Box::new((Mutex::new(false),
                                                                   Condvar::new())));
                unsafe {
                    mpv_set_wakeup_callback(ctx,
                                            event_callback,
                                            ev_iter_notification as *mut _);
                }

                (Some(ev_iter_notification),
                 Some(Mutex::new(Vec::with_capacity(15))),
                 Some(Mutex::new(HashMap::with_capacity(10))),
                 Some(Mutex::new(Vec::with_capacity(22))))
            } else {
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

                (None, None, None, None)
            };

        let instance = Client {
            ctx: ctx,
            check_events: check_events,
            ev_iter_notification: ev_iter_notification,
            ev_to_observe: ev_to_observe,
            ev_to_observe_properties: ev_to_observe_properties,
            ev_observed: ev_observed,
            _does_not_outlive: PhantomData::<&Self>,
        };
        Ok(instance)
    }

    #[inline]
    /// Suspend the playback thread. If the core is suspended, only client API calls will be
    /// accepted, ie. input, redrawing etc. will be suspended.
    /// For the suspended to resume there has to be one `resume` call for each `suspend` call.
    pub fn suspend(&self) {
        *self.suspension_count.lock() += 1;
        unsafe { mpv_suspend(self.ctx()) }
    }

    #[inline]
    /// See `suspend`.
    pub fn resume(&self) -> Result<(), Error> {
        let mut guard = self.suspension_count.lock();
        if *guard == 0 {
            Err(Error::AlreadyResumed)
        } else {
            *guard -= 1;
            Ok(unsafe { mpv_resume(self.ctx()) })
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
        try!(protocol.register(self.ctx));
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

/// Core functionality that is supported by both `Client` and `Parent`.
pub trait MpvInstance<'parent, P>
    where P: MpvMarker + 'parent
{
    /// Load a configuration file.
    fn load_config(&self, path: &Path) -> Result<(), Error>;
    /// Observe all given `Event`s by means of an `EventIter`.
    fn observe_all(&self, events: &[Event]) -> Result<EventIter<P>, Error>;
    /// Execute any mpv command. See implementation for information about safety.
    unsafe fn command(&self, cmd: &Command) -> Result<(), Error>;
    /// Set a given property.
    fn set_property<D: Into<Data>>(&self, name: &str, mut data: D) -> Result<(), Error>;
    /// Get the `Data` of a given property.
    fn get_property(&self, name: &str, format: &Format) -> Result<Data, Error>;
    /// Seek in a way defined by `Seek`.
    fn seek(&self, seek: &Seek) -> Result<(), Error>;
    /// Take a screenshot in a way defined by `Screenshot`.
    fn screenshot(&self, st: &Screenshot) -> Result<(), Error>;
    /// Operate on the playlist in a way defined by `PlaylistOp`.
    fn playlist(&self, op: &PlaylistOp) -> Result<(), Error>;
    /// Operate on the subtitles in a way defined by `SubOp`.
    fn subtitle(&self, op: &SubOp) -> Result<(), Error>;
    /// Add -or subtract- any value from a property. Over/underflow clamps to max/min.
    fn add_property(&self, property: &str, value: isize) -> Result<(), Error>;
    /// Cycle a given named property, either up or down.
    fn cycle_property(&self, property: &str, up: &bool) -> Result<(), Error>;
    /// Multiply a given named property by an unsigned factor.
    fn multiply_property(&self, property: &str, factor: &usize) -> Result<(), Error>;
    /// Pause playback.
    fn pause(&self) -> Result<(), Error>;
    /// Unpause playback.
    fn unpause(&self) -> Result<(), Error>;
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
    /// Observe given `Event`s via an `EventIter`.
    fn observe_all(&self, events: &[Event]) -> Result<EventIter<P>, Error> {
        if self.check_events() {
            let mut observe = self.ev_to_observe().as_ref().unwrap().lock();
            let mut properties = self.ev_to_observe_properties().as_ref().unwrap().lock();

            let mut ids = Vec::with_capacity(events.len());
            let mut evs = Vec::with_capacity(events.len());
            let mut props = Vec::with_capacity(events.len());
            for elem in events {
                if let Event::PropertyChange(ref v) = *elem {
                    if properties.contains_key(&v.0) {
                        return Err(Error::AlreadyObserved(Rc::new(elem.clone())));
                    } else {
                        try!(mpv_err((), unsafe { mpv_request_event(self.ctx(), elem.as_id(), 1) }));
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
                        try!(mpv_err((), unsafe {
                            mpv_request_log_messages(self.ctx(), min_level.as_ptr())
                        }));
                    }

                    try!(mpv_err((), unsafe { mpv_request_event(self.ctx(), elem.as_id(), 1) }));
                    ids.push(elem.as_id());
                    evs.push(elem.clone());
                }
            }

            let mut props_ins = Vec::with_capacity(events.len());
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
                notification: self.ev_iter_notification().unwrap(),
                all_to_observe: self.ev_to_observe().as_ref().unwrap(),
                all_to_observe_properties: self.ev_to_observe_properties().as_ref().unwrap(),
                local_to_observe: evs,
                all_observed: self.ev_observed().as_ref().unwrap(),
                _does_not_outlive: PhantomData::<&Self>,
            })
        } else {
            Err(Error::EventsDisabled)
        }
    }

    #[inline]
    /// Send a command to the `Mpv` instance. This uses `mpv_command_string` internally,
    /// so that the syntax is the same as described in the [manual for the input.conf]
    /// (https://mpv.io/manual/master/#list-of-input-commands).
    ///
    /// # Safety
    /// This method is unsafe because arbitrary code may be executed resulting in UB and more.
    unsafe fn command(&self, cmd: &Command) -> Result<(), Error> {
        let mut args = String::with_capacity(cmd.args.iter()
                                                     .fold(0, |acc, e| acc + e.len() + 1));
        for elem in cmd.args {
            args.push_str(" ");
            args.push_str(elem);
        }
        let args = CString::new(format!("{}{}", cmd.name, args)).unwrap();

        mpv_err((), mpv_command_string(self.ctx(), args.as_ptr()))
    }

    #[inline]
    /// Set the value of a property.
    fn set_property<T: Into<Data>>(&self, name: &str, data: T) -> Result<(), Error> {
        let name = CString::new(name).unwrap().into_raw();
        let mut data = data.into();
        let format = data.format().as_val();
        let ret = match data {
            Data::OsdString(_) => Err(Error::OsdStringWrite),
            Data::String(ref v) => {
                let data = CString::new(v.as_bytes()).unwrap();
                let ptr: *mut _ = &mut data.as_ptr();

                mpv_err((), unsafe { mpv_set_option(self.ctx(), name, format, ptr as *mut _) })
            }
            _ => {
                let data = data_ptr!(&mut data);

                mpv_err((),
                        unsafe { mpv_set_property(self.ctx(), name, format, data) })
            }
        };
        unsafe { CString::from_raw(name) };
        ret
    }

    #[inline]
    /// Get the value of a property.
    fn get_property(&self, name: &str, format: &Format) -> Result<Data, Error> {
        let name = CString::new(name).unwrap();
        match *format {
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

                        let data = utils::mpv_cstr_to_string(ret);

                        unsafe{mpv_free(*ptr as *mut _)}

                        Ok(match *format {
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

    // --- Convenience command functions ---
    //


    #[inline]
    /// Seek as defined by `Seek`.
    fn seek(&self, seek: &Seek) -> Result<(), Error> {
        match *seek {
            Seek::RelativeForward(d) => unsafe {
                self.command(&Command::new("seek",
                                           &[&format!("{}", d.as_secs()), "relative"]))

            },
            Seek::RelativeBackward(d) => unsafe {
                self.command(&Command::new("seek",
                                           &[&format!("-{}", d.as_secs()), "relative"]))
            },
            Seek::Absolute(d) => unsafe {
                self.command(&Command::new("seek",
                                           &[&format!("{}", d.as_secs()), "absolute"]))
            },
            Seek::RelativePercent(p) => {
                if p > 100 {
                    // This is actually allowed in libmpv (seek to end),
                    // but it's confusing and may be an indicator of bugs.
                    Err(Error::OutOfBounds)
                } else {
                    unsafe {
                        self.command(&Command::new("seek",
                                                   &[&format!("{}", p), "relative-percent"]))
                    }
                }

            }
            Seek::AbsolutePercent(p) => {
                if p > 100 {
                    // See `Seek::RelativePercent` above.
                    Err(Error::OutOfBounds)
                } else {
                    unsafe {
                        self.command(&Command::new("seek",
                                                   &[&format!("{}", p), "absolute-percent"]))
                    }
                }
            }
            Seek::Revert => unsafe { self.command(&Command::new("revert-seek", &[])) },
            Seek::RevertMark => unsafe {
                self.command(&Command::new("revert-seek", &["mark"]))
            },
            Seek::Frame => unsafe { self.command(&Command::new("frame-step", &[])) },
            Seek::FrameBack => unsafe { self.command(&Command::new("frame-back-step", &[])) },
        }
    }


    #[inline]
    /// Take a screenshot as defined by `Screenshot`.
    fn screenshot(&self, st: &Screenshot) -> Result<(), Error> {
        match *st {
            Screenshot::Subtitles => unsafe {
                self.command(&Command::new("screenshot", &["subtitles"]))
            },
            Screenshot::SubtitlesFile(p) => unsafe {
                self.command(&Command::new("screenshot",
                                           &[p.to_str().unwrap(), "subtitles"]))
            },
            Screenshot::Video => unsafe {
                self.command(&Command::new("screenshot", &["video"]))
            },
            Screenshot::VideoFile(p) => unsafe {
                self.command(&Command::new("screenshot",
                                           &[p.to_str().unwrap().into(), "video"]))
            },
            Screenshot::Window => unsafe {
                self.command(&Command::new("screenshot", &["window"]))
            },
            Screenshot::WindowFile(p) => unsafe {
                self.command(&Command::new("screenshot",
                                           &[p.to_str().unwrap(), "window"]))
            },
        }
    }

    #[inline]
    /// Execute an operation on the playlist as defined by `PlaylistOp`
    fn playlist(&self, op: &PlaylistOp) -> Result<(), Error> {
        match *op {
            PlaylistOp::NextWeak => unsafe {
                self.command(&Command::new("playlist-next", &["weak"]))
            },
            PlaylistOp::NextForce => unsafe {
                self.command(&Command::new("playlist-next", &["force"]))
            },
            PlaylistOp::PreviousWeak => unsafe {
                self.command(&Command::new("playlist-previous", &["weak"]))
            },
            PlaylistOp::PreviousForce => unsafe {
                self.command(&Command::new("playlist-previous", &["force"]))
            },
            PlaylistOp::LoadlistReplace(p) => unsafe {
                self.command(&Command::new("loadlist",
                                           &[&format!("\"{}\"", p.to_str().unwrap()),
                                             "replace"]))
            },
            PlaylistOp::LoadlistAppend(p) => unsafe {
                self.command(&Command::new("loadlist",
                                           &[&format!("\"{}\"", p.to_str().unwrap()),
                                             "append"]))
            },
            PlaylistOp::Clear => unsafe { self.command(&Command::new("playlist-clear", &[])) },
            PlaylistOp::RemoveCurrent => unsafe {
                self.command(&Command::new("playlist-remove", &["current"]))
            },
            PlaylistOp::RemoveIndex(i) => unsafe {
                self.command(&Command::new("playlist-remove", &[&format!("{}", i)]))
            },
            PlaylistOp::Move((old, new)) => unsafe {
                self.command(&Command::new("playlist-move",
                                           &[&format!("{}", new), &format!("{}", old)]))
            },
            PlaylistOp::Shuffle => unsafe { self.command(&Command::new("playlist-shuffle", &[])) },
            PlaylistOp::Loadfiles(lfiles) => {
                for (i, elem) in lfiles.iter().enumerate() {
                    let args = match elem.options {
                            Some(v) => {
                                [format!("\"{}\"",
                                         elem.path
                                             .to_str()
                                             .unwrap()),
                                 elem.state.val().into(),
                                 v.into()]
                            }
                            None => {
                                [format!("\"{}\"",
                                         elem.path
                                             .to_str()
                                             .unwrap()),
                                 elem.state.val().into(),
                                 "".into()]
                            }};
                    let ret = unsafe {
                        self.command(&Command {
                                name: "loadfile",
                                args: &[&args[0], &args[1], &args[2]],
                    })};
                    if ret.is_err() {
                        return Err(Error::Loadfiles((i, Rc::new(ret.unwrap_err()))));
                    }
                }
                Ok(())
            }
        }
    }

    #[inline]
    /// Execute an operation as defined by `SubOp`.
    fn subtitle(&self, op: &SubOp) -> Result<(), Error> {
        match *op {
            SubOp::AddSelect(p, t, l) => unsafe {
                self.command(&Command::new("sub-add",
                                           &[&format!("\"{}\"", p.to_str().unwrap()),
                                             &format!("select{}{}",
                                                     if t.is_some() {
                                                         format!(" {}", t.unwrap())
                                                     } else {
                                                         "".into()
                                                     },
                                                     if l.is_some() {
                                                         format!(" {}", l.unwrap())
                                                     } else {
                                                         "".into()
                                                     })]))
            },
            SubOp::AddAuto(p, t, l) => unsafe {
                self.command(&Command::new("sub-add",
                                           &[&format!("\"{}\"", p.to_str().unwrap()),
                                             &format!("auto{}{}",
                                                     if t.is_some() {
                                                         format!(" {}", t.unwrap())
                                                     } else {
                                                         "".into()
                                                     },
                                                     if l.is_some() {
                                                         format!(" {}", l.unwrap())
                                                     } else {
                                                         "".into()
                                                     })]))
            },
            SubOp::AddCached(p, t, l) => {
                let t = if t.is_some() {
                            format!(" {}", t.unwrap())
                        } else {
                            "".into()
                        };
                let l = if l.is_some() {
                            format!(" {}", l.unwrap())
                        } else {
                            "".into()
                        };
                unsafe {self.command(&Command::new("sub-add",
                                                   &[&format!("\"{}\"", p.to_str().unwrap()),
                                                   &format!("cached{}{}", t, l)]))}
            },
            SubOp::Remove(i) => {
                let i = if i.is_some() {
                            format!("{}", i.unwrap())
                        } else {
                            "".into()
                        };
                unsafe{self.command(&Command::new("sub-remove", &[&i]))}
            },
            SubOp::Reload(i) => {
                let i = if i.is_some() {
                            format!("{}", i.unwrap())
                        } else {
                            "".into()
                        };
                unsafe{self.command(&Command::new("sub-reload", &[&i]))}
            },
            SubOp::Step(i) => unsafe {
                self.command(&Command::new("sub-step", &[&format!("{}", i)]))
            },
            SubOp::SeekForward => unsafe { self.command(&Command::new("sub-seek", &["1"])) },
            SubOp::SeekBackward => unsafe {
                self.command(&Command::new("sub-seek", &["-1"]))
            },
        }
    }

    // --- Convenience property functions ---
    //
    

    #[inline]
    /// Add -or subtract- any value from a property. Over/underflow clamps to max/min.
    fn add_property(&self, property: &str, value: isize) -> Result<(), Error> {
        unsafe { self.command(&Command::new("add", &[property, &format!("{}", value)])) }
    }

    #[inline]
    /// Cycle through a given property. `up` specifies direction. On
    /// overflow, set the property back to the minimum, on underflow set it to the maximum.
    fn cycle_property(&self, property: &str, up: &bool) -> Result<(), Error> {
        unsafe {
            self.command(&Command::new("cycle",
                                       &[property,
                                         if *up {
                                             "up"
                                         } else {
                                             "down"
                                         }]))
        }
    }

    #[inline]
    /// Multiply any property with any positive factor.
    fn multiply_property(&self, property: &str, factor: &usize) -> Result<(), Error> {
        unsafe {
            self.command(&Command::new("multiply", &[property, &format!("{}", factor)]))
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
}
