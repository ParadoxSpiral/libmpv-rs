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

/// Contains event related things
pub mod events;
/// Contains miscellaneous things
#[macro_use]
pub mod utils;

use libc;
use parking_lot::{Condvar, Mutex, Once, ONCE_INIT};

use super::raw::*;
use super::raw::prototype::*;
use events::*;
use events::event_callback;
use utils::*;
use utils::mpv_err;

use std::collections::HashMap;
use std::marker::PhantomData;
use std::path::Path;
use std::ptr;
use std::ffi::{CStr, CString};
use std::ops::Drop;
use std::time::Duration;

#[cfg(unix)]
use std::ffi::OsStr;
#[cfg(unix)]
use std::os::unix::ffi::OsStrExt;

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
    Loadfiles((usize, Box<Error>)),
    /// Events are not enabled for this `Mpv` instance.
    EventsDisabled,
    /// This event is already being observed by another `EventIter`.
    AlreadyObserved(Box<Event>),
    /// Used a `Data::OsdString` while writing.
    OsdStringWrite,
    /// Mpv returned a string that uses an unsupported codec. Inside are the raw bytes cast to u8.
    UnsupportedEncoding(Vec<u8>),
    /// The library was compiled against a different mpv version than what is present on the system.
    VersionMismatch(u32),
    /// Mpv returned null while creating the core, or if `get_property with` `Format::String` fails.
    Null,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(missing_docs)]
/// Data that can be sent to or retrieved from `Mpv`.
pub enum Data {
    String(String),
    OsdString(String),
    Flag(bool),
    Int64(libc::int64_t),
    Double(libc::c_double),
    Node(MpvNode),
}

impl Data {
    #[inline]
    /// Create a `Data` from a supported value.
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
            Data::Node(_) => MpvFormat::Node,
        }
    }

    fn from_raw(fmt: MpvFormat, ptr: *mut libc::c_void) -> Data {
        debug_assert!(!ptr.is_null());
        match fmt {
            MpvFormat::Flag => Data::Flag(unsafe { *(ptr as *mut libc::int64_t) } != 0),
            MpvFormat::Int64 => Data::Int64(unsafe { *(ptr as *mut libc::int64_t) }),
            MpvFormat::Double => Data::Double(unsafe { *(ptr as *mut libc::c_double) }),
            MpvFormat::Node => Data::Node(unsafe { *(ptr as *mut MpvNode) }),
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

impl Into<Data> for bool {
    #[inline]
    fn into(self) -> Data {
        Data::Flag(self)
    }
}

impl Into<Data> for i32 {
    #[inline]
    fn into(self) -> Data {
        Data::Int64(self as libc::int64_t)
    }
}

impl Into<Data> for i64 {
    #[inline]
    fn into(self) -> Data {
        Data::Int64(self as libc::int64_t)
    }
}

impl Into<Data> for u32 {
    #[inline]
    fn into(self) -> Data {
        Data::Int64(self as libc::int64_t)
    }
}

impl Into<Data> for f32 {
    #[inline]
    fn into(self) -> Data {
        Data::Double(self as libc::c_double)
    }
}

impl Into<Data> for f64 {
    #[inline]
    fn into(self) -> Data {
        Data::Double(self as libc::c_double)
    }
}

impl Into<Data> for MpvNode {
    #[inline]
    fn into(self) -> Data {
        Data::Node(self)
    }
}

#[derive(Clone, Debug)]
#[allow(missing_docs)]
/// Contains name and data of a property.
///
/// Partial equality only implies that the names are equal.
pub struct Property {
    pub name: String,
    pub data: Data,
}

impl Property {
    fn from_raw(raw: *mut libc::c_void) -> Property {
        debug_assert!(!raw.is_null());
        let raw = unsafe { &mut *(raw as *mut MpvEventProperty) };
        Property {
            name: unsafe { CStr::from_ptr(raw.name).to_str().unwrap().into() },
            data: Data::from_raw(raw.format, raw.data),
        }
    }

    #[inline]
    /// Create a `Property` that is suitable for observing.
    /// Data is used to infer the format of the property, the value is never used in this case.
    pub fn new(name: &str, data: Data) -> Property {
        Property {
            name: name.into(),
            data: data,
        }
    }
}

impl PartialEq<Property> for Property {
    #[inline]
    fn eq(&self, other: &Property) -> bool {
        self.name == other.name
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
/// Possible seek operations by `seek`.
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
/// Possible screenshot operations by `screenshot`.
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
    /// See `screenshot_subtitles_to_file`.
    VideoFile(&'a Path),
    /// "Save the contents of the mpv window. Typically scaled, with OSD
    /// and subtitles. The exact behaviour depends on the selected video output, and if no support
    /// is available, this will act like video.".
    Window,
    /// See `screenshot_subtitles_to_file`.
    WindowFile(&'a Path),
}

#[derive(Clone, Copy, Debug)]
/// Operations on the playlist supported by `playlist`.
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

/// This type allows for operations that should only be done on an uninitialized mpv core.
pub struct UninitializedParent {
    ctx: *mut MpvHandle,
    check_events: bool,
    drop_do_destroy: bool,
}

impl UninitializedParent {
    #[inline]
    /// Initialize the mpv core, return an initialized `Parent`.
    pub fn init(mut self) -> Result<Parent, Error> {
        self.drop_do_destroy = false;
        unsafe { destroy_on_err!(self.ctx, mpv_initialize(self.ctx)) }
        Parent::from_uninitialized(self)
    }

    #[inline]
    /// Create a new `UninitializedParent` instance.
    pub fn new(check_events: bool) -> Result<UninitializedParent, Error> {
        SET_LC_NUMERIC.call_once(|| {
            let c = CString::new("C").unwrap();
            unsafe { libc::setlocale(libc::LC_NUMERIC, c.as_ptr()) };
        });

        let api_version = unsafe { mpv_client_api_version() };
        if super::MPV_CLIENT_API_VERSION != api_version {
            return Err(Error::VersionMismatch(api_version));
        }

        let ctx = unsafe { mpv_create() };
        if ctx == ptr::null_mut() {
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

            if !check_events {
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

        Ok(UninitializedParent {
            ctx: ctx,
            check_events: check_events,
            drop_do_destroy: true,
        })
    }

    #[inline]
    /// Set an option.
    pub fn set_option(&self, opt: &mut Property) -> Result<(), Error> {
        let name = CString::new(&opt.name[..]).unwrap().into_raw();
        let format = opt.data.format().as_val();
        let ret = match opt.data {
            Data::OsdString(_) => Err(Error::OsdStringWrite),
            Data::String(ref v) => {
                let data = CString::new(v.as_bytes()).unwrap().into_raw();

                let ret = mpv_err((), unsafe {
                    mpv_set_option(self.ctx, name, format, data as *mut libc::c_void)
                });
                unsafe {
                    CString::from_raw(data);
                };
                ret
            }
            _ => {
                let data = &mut opt.data;
                let data = data_ptr!(data);

                mpv_err((), unsafe { mpv_set_option(self.ctx, name, format, data) })
            }
        };
        unsafe { CString::from_raw(name) };
        ret
    }

    #[inline]
    /// Load a configuration file into the `Mpv` instance.
    /// The path has to be absolute, and a file.
    pub fn load_config(&self, path: &Path) -> Result<(), Error> {
        if path.is_relative() {
            Err(Error::ExpectedAbsolute)
        } else if !path.is_file() {
            Err(Error::ExpectedFile)
        } else {
            let file = CString::new(path.to_str().unwrap()).unwrap().into_raw();
            let ret = mpv_err((), unsafe { mpv_load_config_file(self.ctx, file) });
            unsafe { CString::from_raw(file) };
            ret
        }
    }
}

impl Drop for UninitializedParent {
    #[inline]
    fn drop(&mut self) {
        if self.drop_do_destroy {
            unsafe {
                mpv_terminate_destroy(self.ctx);
            }
        }
    }
}

/// An mpv instance from which `Client`s can be spawned.
///
/// # Panics
/// Any method on this struct may panic if any argument contains invalid utf-8.
pub struct Parent {
    ctx: *mut MpvHandle,
    suspension_count: Mutex<usize>,
    check_events: bool,
    ev_iter_notification: Option<*mut (Mutex<bool>, Condvar)>,
    ev_to_observe: Option<Mutex<Vec<Event>>>,
    ev_to_observe_properties: Option<Mutex<HashMap<String, libc::uint64_t>>>,
    ev_observed: Option<Mutex<Vec<InnerEvent>>>,
}

/// A client of a `Parent`.
///
/// # Panics
/// Any method on this struct may panic if any argument contains invalid utf-8.
pub struct Client<'parent> {
    ctx: *mut MpvHandle,
    check_events: bool,
    ev_iter_notification: Option<*mut (Mutex<bool>, Condvar)>,
    ev_to_observe: Option<Mutex<Vec<Event>>>,
    ev_observed: Option<Mutex<Vec<InnerEvent>>>,
    ev_to_observe_properties: Option<Mutex<HashMap<String, libc::uint64_t>>>,
    _does_not_outlive: PhantomData<&'parent Parent>,
}

unsafe impl Send for Parent {}
unsafe impl Sync for Parent {}
unsafe impl<'parent> Send for Client<'parent> {}
unsafe impl<'parent> Sync for Client<'parent> {}

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
    fn drop_ev_iter_step(&mut self) {
        if self.check_events() {
            unsafe {
                Box::from_raw(self.ev_iter_notification().unwrap());
            }
        }
    }
}

unsafe impl MpvMarker for Parent {
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

unsafe impl<'parent> MpvMarker for Client<'parent> {
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

impl Drop for Parent {
    #[inline]
    fn drop(&mut self) {
        self.drop_ev_iter_step();
        unsafe {
            mpv_terminate_destroy(self.ctx());
        }
    }
}

impl<'parent> Drop for Client<'parent> {
    #[inline]
    fn drop(&mut self) {
        self.drop_ev_iter_step();
        unsafe {
            mpv_detach_destroy(self.ctx());
        }
    }
}

impl<'parent> Parent {
    #[inline]
    /// Create a new `Mpv` instance.
    /// To call any method except for `set_option` on this, it has to be initialized first.
    /// The default settings can be probed by running: ```$ mpv --show-profile=libmpv```
    pub fn new(check_events: bool) -> Result<Parent, Error> {
        SET_LC_NUMERIC.call_once(|| {
            let c = CString::new("C").unwrap();
            unsafe { libc::setlocale(libc::LC_NUMERIC, c.as_ptr()) };
        });

        let api_version = unsafe { mpv_client_api_version() };
        if super::MPV_CLIENT_API_VERSION != api_version {
            return Err(Error::VersionMismatch(api_version));
        }

        let ctx = unsafe { mpv_create() };
        if ctx == ptr::null_mut() {
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
                                            ev_iter_notification as *mut libc::c_void);
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
        })
    }

    fn from_uninitialized(uninit: UninitializedParent) -> Result<Parent, Error> {
        let (ev_iter_notification, ev_to_observe, ev_to_observe_properties, ev_observed) =
            if uninit.check_events {
                let ev_iter_notification = Box::into_raw(Box::new((Mutex::new(false),
                                                                   Condvar::new())));
                unsafe {
                    mpv_set_wakeup_callback(uninit.ctx,
                                            event_callback,
                                            ev_iter_notification as *mut libc::c_void);
                }

                (Some(ev_iter_notification),
                 Some(Mutex::new(Vec::with_capacity(15))),
                 Some(Mutex::new(HashMap::with_capacity(10))),
                 Some(Mutex::new(Vec::with_capacity(22))))
            } else {
                (None, None, None, None)
            };

        Ok(Parent {
            ctx: uninit.ctx,
            suspension_count: Mutex::new(0),
            check_events: uninit.check_events,
            ev_iter_notification: ev_iter_notification,
            ev_to_observe: ev_to_observe,
            ev_to_observe_properties: ev_to_observe_properties,
            ev_observed: ev_observed,
        })
    }

    #[inline]
    /// Create a client with `name`, that is connected to the core of `self`, but has an own queue
    /// for API events and such.
    pub fn new_client(&self, name: &str, check_events: bool) -> Result<Client, Error> {
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
                                            ev_iter_notification as *mut libc::c_void);
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
    pub fn suspend(&self) -> Result<(), Error> {
        *self.suspension_count.lock() += 1;
        Ok(unsafe { mpv_suspend(self.ctx()) })
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
}

impl<'parent> Client<'parent> {
    #[inline]
    /// Returns the name associated with the instance.
    pub fn name(&self) -> &str {
        unsafe { CStr::from_ptr(mpv_client_name(self.ctx())).to_str().unwrap() }
    }
}

/// Core functionality that is supported by both `Client` and `Parent`.
///
/// # Panics
/// Any method may panic if any argument contains invalid utf-8.
pub trait MpvInstance<'parent, P>
    where P: MpvMarker + 'parent
{
    /// Observe all `Event`s by means of an `EventIter`.
    fn observe_all(&self, events: &[Event]) -> Result<EventIter<P>, Error>;
    /// Execute any mpv command. See implementation for information about safety.
    unsafe fn command(&self, cmd: &Command) -> Result<(), Error>;
    /// Set a given `Property` with `prop`, using it's value.
    fn set_property(&self, prop: &mut Property) -> Result<(), Error>;
    /// Get the `Data` of a given named property.
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
                    if properties.contains_key(&v.name) {
                        return Err(Error::AlreadyObserved(Box::new(elem.clone())));
                    } else {
                        try!(mpv_err((), unsafe { mpv_request_event(self.ctx(), elem.as_id(), 1) }));
                        props.push(v);
                        ids.push(elem.as_id());
                        evs.push(elem.clone());
                    }
                } else {
                    for id in &*observe {
                        if elem.as_id() == id.as_id() {
                            return Err(Error::AlreadyObserved(Box::new(elem.clone())));
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
                unsafe {
                    let name = CString::new(elem.name.clone()).unwrap();
                    let err = mpv_err((),
                                      mpv_observe_property(self.ctx(),
                                                           (start_id + i) as libc::uint64_t,
                                                           name.as_ptr(),
                                                           elem.data.format() as libc::c_int));
                    if err.is_err() {
                        for (_, id) in props_ins {
                            // Ignore errors.
                            mpv_unobserve_property(self.ctx(), id);
                        }
                        return Err(err.unwrap_err());
                    }
                }
                props_ins.push((elem.name.clone(), (start_id + i) as libc::uint64_t));
            }
            observe.extend(evs.clone());
            properties.extend(props_ins);

            Ok(EventIter {
                ctx: self.ctx(),
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
                                                     .fold(0, |acc, ref e| acc + e.len() + 1));
        for elem in cmd.args {
            args.push_str(" ");
            args.push_str(elem);
        }
        let args = CString::new(format!("{}{}", cmd.name, args)).unwrap();

        mpv_err((), mpv_command_string(self.ctx(), args.as_ptr()))
    }

    #[inline]
    /// Set the value of a property.
    fn set_property(&self, prop: &mut Property) -> Result<(), Error> {
        let format = prop.data.format().as_val();
        let name = CString::new(&prop.name[..]).unwrap().into_raw();
        let ret = match prop.data {
            Data::OsdString(_) => Err(Error::OsdStringWrite),
            Data::String(ref v) => {
                let data = CString::new(v.as_bytes()).unwrap().into_raw();

                let ret = mpv_err((), unsafe {
                    mpv_set_property(self.ctx(), name, format, data as *mut libc::c_void)
                });
                unsafe {
                    CString::from_raw(data);
                };
                ret
            }
            _ => {
                let data = &mut prop.data;
                let data = data_ptr!(data);

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
                let mut ptr = &mut ptr::null::<libc::c_char>();

                let err = mpv_err((), unsafe {
                    mpv_get_property(self.ctx(),
                                     name.as_ptr(),
                                     format.as_mpv_format().as_val(),
                                     ptr as *mut *const libc::c_char as *mut libc::c_void)
                });
                debug_assert!(!ptr.is_null());

                err.or_else(Err)
                    .and_then(|_| {
                        let ret = unsafe { CStr::from_ptr(*ptr) };

                        let data;
                        #[cfg(windows)] {
                            // Mpv returns all strings on windows in UTF-8.
                            data = ret.to_str().unwrap().to_owned();
                        }
                        #[cfg(unix)] {
                            data = OsStr::from_bytes(ret.to_bytes()).to_string_lossy().into_owned();
                        }
                        #[cfg(all(not(unix), not(windows)))] {
                            // Hope that all is well
                            data = String::from_utf8_lossy(ret.to_bytes()).into_owned();
                        }

                        unsafe{mpv_free(*ptr as *mut libc::c_void)}

                        Ok(match *format {
                            Format::String => Data::String(data),
                            Format::OsdString => Data::OsdString(data),
                            _ => unreachable!(),
                        })
                    })
            }
            _ => {
                let ptr: *mut libc::c_void = unsafe { libc::malloc(format.size()) };

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
            Screenshot::SubtitlesFile(ref p) => unsafe {
                self.command(&Command::new("screenshot",
                                           &[p.to_str().unwrap(), "subtitles"]))
            },
            Screenshot::Video => unsafe {
                self.command(&Command::new("screenshot", &["video"]))
            },
            Screenshot::VideoFile(ref p) => unsafe {
                self.command(&Command::new("screenshot",
                                           &[p.to_str().unwrap().into(), "video"]))
            },
            Screenshot::Window => unsafe {
                self.command(&Command::new("screenshot", &["window"]))
            },
            Screenshot::WindowFile(ref p) => unsafe {
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
                    let ret = unsafe {
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
                                        }
                                    };
                        self.command(&Command {
                            name: "loadfile",
                            args: &[&args[0], &args[1], &args[2]],
                        })
                    };
                    if ret.is_err() {
                        return Err(Error::Loadfiles((i, Box::new(ret.unwrap_err()))));
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
            SubOp::AddCached(p, t, l) => unsafe {
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
                self.command(&Command::new("sub-add",
                                           &[&format!("\"{}\"", p.to_str().unwrap()),
                                             &format!("cached{}{}", t, l)]))
            },
            SubOp::Remove(i) => unsafe {
                let i = if i.is_some() {
                            format!("{}", i.unwrap())
                        } else {
                            "".into()
                        };
                self.command(&Command::new("sub-remove", &[&i]))
            },
            SubOp::Reload(i) => unsafe {
                let i = if i.is_some() {
                            format!("{}", i.unwrap())
                        } else {
                            "".into()
                        };
                self.command(&Command::new("sub-reload", &[&i]))
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
        self.set_property(&mut Property::new("pause", Data::Flag(true)))
    }

    #[inline]
    /// Unpause playback at runtime.
    fn unpause(&self) -> Result<(), Error> {
        self.set_property(&mut Property::new("pause", Data::Flag(false)))
    }
}
