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

#![allow(unknown_lints)]

use libc;
use encoding;
use parking_lot::{Condvar, Mutex};
use enum_primitive::FromPrimitive;

use super::raw::*;
use super::raw::prototype::*;

use std::boxed::Box;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::mem;
use std::path::Path;
use std::ptr;
use std::ffi::{CStr, CString};
use std::ops::Drop;
use std::time::Duration;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

// Cast `Data` so that libmpv can use it.
macro_rules! data_ptr {
    ($data:ident) => (
        #[allow(match_ref_pats)]
        match $data {
            &mut Data::Flag(ref mut v) =>
                v as *mut bool as *mut libc::c_void,
            &mut Data::Int64(ref mut v) =>
                v as *mut libc::int64_t as *mut libc::c_void,
            &mut Data::Double(ref mut v) =>
                v as *mut libc::c_double as *mut libc::c_void,
            &mut Data::Node(ref mut v) =>
                v as *mut MpvNode as *mut libc::c_void,
            _ => unreachable!(),
        }
    )
}

pub(crate) fn mpv_err<T>(ret: T, err_val: libc::c_int) -> Result<T, Error> {
    if err_val == 0 {
        Ok(ret)
    } else {
        Err(Error::Mpv(MpvError::from_i32(err_val).unwrap()))
    }
}

unsafe extern "C" fn event_callback(d: *mut libc::c_void) {
    (*(d as *mut (Mutex<bool>, Condvar))).1.notify_one();
}

#[doc(hidden)]
#[derive(Clone, Debug, PartialEq)]
/// Designed for internal use.
pub struct InnerEvent {
    event: Event,
    err: Option<Error>,
}

impl InnerEvent {
    fn as_result(&self) -> Result<Event, Error> {
        if self.err.is_some() {
            Err(self.err.clone().unwrap())
        } else {
            Ok(self.event.clone())
        }
    }
    fn as_event(&self) -> &Event {
        &self.event
    }
}

/// An event returned by `EventIter`.
#[derive(Clone, Debug, PartialEq)]
#[allow(missing_docs)]
pub enum Event {
    LogMessage(Option<LogMessage>),
    StartFile,
    EndFile(Option<EndFile>),
    FileLoaded,
    Idle,
    Tick,
    VideoReconfig,
    AudioReconfig,
    Seek,
    PlaybackRestart,
    PropertyChange(Property),
}

impl Event {
    fn as_id(&self) -> MpvEventId {
        match *self {
            Event::LogMessage(_) => MpvEventId::LogMessage,
            Event::StartFile => MpvEventId::StartFile,
            Event::EndFile(_) => MpvEventId::EndFile,
            Event::FileLoaded => MpvEventId::FileLoaded,
            Event::Idle => MpvEventId::Idle,
            Event::Tick => MpvEventId::Tick,
            Event::VideoReconfig => MpvEventId::VideoReconfig,
            Event::AudioReconfig => MpvEventId::AudioReconfig,
            Event::Seek => MpvEventId::Seek,
            Event::PlaybackRestart => MpvEventId::PlaybackRestart,
            Event::PropertyChange(_) => MpvEventId::PropertyChange,
        }
    }
}

impl MpvEvent {
    fn as_event(&self) -> Result<Event, Error> {
        try!(mpv_err((), self.error));
        Ok(match self.event_id {
            MpvEventId::LogMessage => Event::LogMessage(Some(LogMessage::from_raw(self.data))),
            MpvEventId::StartFile => Event::StartFile,
            MpvEventId::EndFile => {
                Event::EndFile(Some(EndFile::from_raw(MpvEventEndFile::from_raw(self.data))))
            }
            MpvEventId::FileLoaded => Event::FileLoaded,
            MpvEventId::Idle => Event::Idle,
            MpvEventId::Tick => Event::Tick,
            MpvEventId::VideoReconfig => Event::VideoReconfig,
            MpvEventId::AudioReconfig => Event::AudioReconfig,
            MpvEventId::Seek => Event::Seek,
            MpvEventId::PlaybackRestart => Event::PlaybackRestart,
            MpvEventId::PropertyChange => Event::PropertyChange(Property::from_raw(self.data)),
            _ => unreachable!(),
        })
    }
    fn as_inner_event(&self) -> InnerEvent {
        InnerEvent {
            event: match self.event_id {
                MpvEventId::LogMessage => Event::LogMessage(Some(LogMessage::from_raw(self.data))),
                MpvEventId::StartFile => Event::StartFile,
                MpvEventId::EndFile => {
                    Event::EndFile(Some(EndFile::from_raw(MpvEventEndFile::from_raw(self.data))))
                }
                MpvEventId::FileLoaded => Event::FileLoaded,
                MpvEventId::Idle => Event::Idle,
                MpvEventId::Tick => Event::Tick,
                MpvEventId::VideoReconfig => Event::VideoReconfig,
                MpvEventId::AudioReconfig => Event::AudioReconfig,
                MpvEventId::Seek => Event::Seek,
                MpvEventId::PlaybackRestart => Event::PlaybackRestart,
                MpvEventId::PropertyChange => Event::PropertyChange(Property::from_raw(self.data)),
                _ => unreachable!(),
            },
            err: {
                let err = mpv_err((), self.error);
                if err.is_err() {
                    Some(err.unwrap_err())
                } else {
                    None
                }
            },
        }
    }
}

/// A blocking iter over some observed events of an mpv instance.
/// `next` will never return `None`, instead it will return `Error::NoAssociatedEvent`. This is done
/// so that the iterator is endless. Once the `EventIter` is dropped, it's `Event`s are removed from
/// the "to be observed" queue, therefore new `Event` invocations won't be observed.
pub struct EventIter<'parent, P>
    where P: MpvMarker + 'parent
{
    ctx: *mut MpvHandle,
    notification: *mut (Mutex<bool>, Condvar),
    all_to_observe: &'parent Mutex<Vec<Event>>,
    all_to_observe_properties: &'parent Mutex<HashMap<String, libc::uint64_t>>,
    local_to_observe: Vec<Event>,
    all_observed: &'parent Mutex<Vec<InnerEvent>>,
    last_no_associated_ev: bool,
    _does_not_outlive: PhantomData<&'parent P>,
}

impl<'parent, P> Drop for EventIter<'parent, P>
    where P: MpvMarker + 'parent
{
    fn drop(&mut self) {
        let mut all_to_observe = self.all_to_observe.lock();
        let mut all_observed = self.all_observed.lock();
        let mut all_to_observe_properties = self.all_to_observe_properties.lock();

        // Returns true if outer and inner event match, in the case of the event
        // being a property, unobserve it.
        let mut compare_ev_unobserve = |outer_ev: &Event, inner_ev: &Event| -> bool {
            if let Event::PropertyChange(ref outer_prop) = *outer_ev {
                if let Event::PropertyChange(ref inner_prop) = *inner_ev {
                    if outer_prop.name == inner_prop.name {
                        unsafe {
                            mpv_unobserve_property(self.ctx, *all_to_observe_properties.get(
                                                                        &outer_prop.name).unwrap());
                        }
                        all_to_observe_properties.remove(&outer_prop.name);
                        return true;
                    }
                }
            } else if outer_ev.as_id() == inner_ev.as_id() {
                return true;
            }
            false
        };

        // This removes all events for which compare_ev_unobserve returns true.
        let mut new_to = Vec::with_capacity(all_to_observe.len());
        let mut new_obd = Vec::with_capacity(all_observed.len());
        for outer_ev in &self.local_to_observe {
            for elem in all_to_observe.iter()
                                      .skip_while(|inner_ev| {
                                          compare_ev_unobserve(outer_ev, *inner_ev)
                                      }) {
                new_to.push(elem.clone());
            }
            for elem in all_observed.iter()
                                    .skip_while(|inner_ev| {
                                        compare_ev_unobserve(outer_ev, (**inner_ev).as_event())
                                    }) {
                new_obd.push(elem.clone());
            }
        }
        *all_to_observe = new_to;
        *all_observed = new_obd;
    }
}

impl<'parent, P> Iterator for EventIter<'parent, P>
    where P: MpvMarker + 'parent
{
    type Item = Result<Vec<Result<Event, Error>>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut observed = self.all_observed.lock();
        if observed.is_empty() || self.last_no_associated_ev {
            mem::drop(observed);
            unsafe { (*self.notification).1.wait(&mut (*self.notification).0.lock()) };
            observed = self.all_observed.lock();
        }

        let mut ret_events = Vec::with_capacity(observed.len());
        if observed.is_empty() {
            let all_to_observe = self.all_to_observe.lock();
            let mut last = false;
            'events: loop {
                let event = unsafe { &*mpv_wait_event(self.ctx, 0f64 as libc::c_double) };
                let ev_id = event.event_id;

                if ev_id == MpvEventId::None || ev_id == MpvEventId::QueueOverflow {
                    if last {
                        break;
                    } else {
                        last = true;
                        continue;
                    }
                }
                for local_ob_ev in &self.local_to_observe {
                    if ev_id == local_ob_ev.as_id() {
                        ret_events.push(event.as_event());
                        continue 'events;
                    }
                }
                for all_ob_ev in &*all_to_observe {
                    if ev_id == all_ob_ev.as_id() {
                        observed.push(event.as_inner_event());
                        continue 'events;
                    }
                }
            }
            if !observed.is_empty() {
                // Dropping early means less spinning in the notified iter, and parking_lot is
                // biased towards uncontended locks
                mem::drop(observed);
                unsafe { (*self.notification).1.notify_all() };
            }
        } else {
            // TODO: Simplify this
            let mut index = Vec::with_capacity(observed.len());
            for (i, event) in observed.iter().enumerate() {
                for o_e_id in &self.local_to_observe {
                    if event.event.as_id() == o_e_id.as_id() {
                        if o_e_id.as_id() == MpvEventId::PropertyChange {
                            if let Event::PropertyChange(ref v_ev) = event.event {
                                if let Event::PropertyChange(ref v_ob) = *o_e_id {
                                    if v_ev.name == v_ob.name {
                                        index.push(i);
                                        ret_events.push(event.as_result());
                                    }
                                }
                            }
                        } else {
                            index.push(i);
                            ret_events.push(event.as_result());
                        }
                    }
                }
            }
            for (n, i) in index.iter().enumerate() {
                observed.remove(i - n);
            }
            if !observed.is_empty() {
                mem::drop(observed);
                unsafe { (*self.notification).1.notify_all() };
            }
        }
        if !ret_events.is_empty() {
            self.last_no_associated_ev = false;
            Some(Ok(ret_events))
        } else {
            self.last_no_associated_ev = true;
            Some(Err(Error::NoAssociatedEvent))
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[allow(missing_docs)]
/// The data of an `Event::LogMessage`.
pub struct LogMessage {
    pub prefix: String,
    pub level: String,
    pub text: String,
    pub log_level: MpvLogLevel,
}

impl LogMessage {
    fn from_raw(raw: *mut libc::c_void) -> LogMessage {
        let raw = unsafe { &mut *(raw as *mut MpvEventLogMessage) };
        LogMessage {
            prefix: unsafe { CStr::from_ptr(raw.prefix).to_str().unwrap().into() },
            level: unsafe { CStr::from_ptr(raw.level).to_str().unwrap().into() },
            text: unsafe { CStr::from_ptr(raw.text).to_str().unwrap().into() },
            log_level: raw.log_level,
        }
    }
}

impl MpvEventEndFile {
    fn from_raw(raw: *mut libc::c_void) -> MpvEventEndFile {
        let raw = unsafe { &mut *(raw as *mut MpvEventEndFile) };
        MpvEventEndFile {
            reason: raw.reason,
            error: raw.error,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(missing_docs)]
/// The reason an `Event::EndFile` was fired.
pub enum EndFileReason {
    Eof = 0,
    Stop = 2,
    Quit = 3,
    Error = 4,
    Redirect = 5,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(missing_docs)]
/// The data of an `Event::EndFile`. `error` is `Some` if `EndFileReason` is `Error`.
pub struct EndFile {
    pub reason: EndFileReason,
    pub error: Option<Error>,
}

impl EndFile {
    fn from_raw(raw: MpvEventEndFile) -> EndFile {
        EndFile {
            reason: match raw.reason {
                0 => EndFileReason::Eof,
                2 => EndFileReason::Stop,
                3 => EndFileReason::Quit,
                4 => EndFileReason::Error,
                5 => EndFileReason::Redirect,
                _ => unreachable!(),

            },
            error: {
                let err = mpv_err((), raw.error);
                if err.is_ok() {
                    None
                } else {
                    Some(err.unwrap_err())
                }
            },
        }
    }
}

#[derive(Clone, Debug)]
#[allow(missing_docs)]
/// The data of an `Event::PropertyChange`. The `data` field is equal to the value of
/// the property.
///
/// Partial equality only imples that only the names are equal.
pub struct Property {
    pub name: String,
    pub data: Data,
}

impl Property {
    fn from_raw(raw: *mut libc::c_void) -> Property {
        let raw = unsafe { &mut *(raw as *mut MpvEventProperty) };
        Property {
            name: unsafe { CStr::from_ptr(raw.name).to_str().unwrap().into() },
            data: Data::from_raw(raw.format, raw.data),
        }
    }

    #[inline]
    /// Create a `Property` that is suitable for observing.
    /// Data is used to infer the format of the property, and the value is never used if supplied to
    /// a function of this crate.
    pub fn new(name: &str, data: Data) -> Property {
        Property {
            name: name.into(),
            data: data,
        }
    }
}

impl PartialEq<Property> for Property {
    fn eq(&self, other: &Property) -> bool {
        self.name == other.name
    }
}

#[derive(Clone, Debug, PartialEq)]
/// All possible error values returned by this crate.
pub enum Error {
    /// An internal mpv error.
    Mpv(MpvError),
    /// The core has already been initialized.
    /// This error is also handled by mpv, but results in a failed assertion.
    AlreadyInitialized,
    /// Calling `suspend` on an uninitialized core will deadlock.
    Uninitialized,
    /// All `suspend` calls have already been undone.
    AlreadyResumed,
    /// Some functions only accept absolute paths.
    ExpectedAbsolute,
    /// If a file was expected, but a directory was given.
    ExpectedFile,
    /// The parent was dropped before the clients
    ParentDropped,
    /// If an argument (like a percentage > 100) was out of bounds.
    OutOfBounds,
    /// If a command failed during a `loadfiles` call, contains index of failed command and `Error`.
    Loadfiles((usize, Box<Error>)),
    /// Events are not enabled for this `Mpv` instance.
    EventsDisabled,
    /// This event is already being observed by another `EventIter`.
    AlreadyObserved(Box<Event>),
    /// No `Event` associated with this `EventIter` was found, this means a spurious wakeup.
    NoAssociatedEvent,
    /// Used a `Data::OsdString` while writing.
    OsdStringWrite,
    /// Mpv returned a string that uses an unsupported codec. Inside are the raw bytes cast to u8.
    UnsupportedEncoding(Vec<u8>),
    /// The library was compiled against a different mpv version than what is present on the system.
    VersionMismatch(u32),
    /// Mpv returned null while creating the core.
    Null,
}

#[derive(Clone, Debug, PartialEq)]
#[allow(missing_docs)]
/// Data that can be sent to or retrieved from `Mpv`.
pub enum Data {
    String(String),
    OsdString(String),
    Flag(bool),
    Int64(i64),
    Double(f64),
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
        match fmt {
            MpvFormat::Flag => Data::Flag(unsafe { *(ptr as *mut i64) } != 0),
            MpvFormat::Int64 => Data::Int64(unsafe { *(ptr as *mut i64) }),
            MpvFormat::Double => Data::Double(unsafe { *(ptr as *mut f64) }),
            // TODO: MpvFormat::Node => Data::Node(unsafe{ *(ptr as *mut MpvNode) }),
            _ => unreachable!(),
        }
    }
}

impl Into<Data> for String {
    fn into(self) -> Data {
        Data::String(self)
    }
}

impl Into<Data> for bool {
    fn into(self) -> Data {
        Data::Flag(self)
    }
}

impl Into<Data> for isize {
    fn into(self) -> Data {
        Data::Int64(self as i64)
    }
}

impl Into<Data> for f64 {
    fn into(self) -> Data {
        Data::Double(self)
    }
}

impl Into<Data> for MpvNode {
    fn into(self) -> Data {
        Data::Node(self)
    }
}

#[derive(Clone, Debug)]
/// A command that can be executed by `Mpv`.
pub struct Command<'a> {
    name: &'a str,
    args: &'a [String],
}

impl<'a> Command<'a> {
    #[inline]
    /// Create a new `MpvCommand`.
    pub fn new(name: &'a str, args: &'a [String]) -> Command<'a> {
        Command {
            name: name,
            args: args,
        }
    }
}

#[derive(Clone, Debug)]
/// Data needed for `PlaylistOp::Loadfiles`.
pub struct File<'a> {
    path: &'a Path,
    state: FileState,
    options: Option<&'a str>,
}

impl<'a> File<'a> {
    #[inline]
    /// Create a new `File`.
    pub fn new(path: &'a Path, state: FileState, opts: Option<&'a str>) -> File<'a> {
        File {
            path: path,
            state: state,
            options: opts,
        }
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

#[derive(Clone, Debug)]
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

#[allow(missing_docs)]
/// Equivalent to subset of `MpvFormat` used by the public API.
pub enum Format {
    String = 1,
    OsdString = 2,
    Flag = 3,
    Int64 = 4,
    Double = 5,
    Node = 6,
}

impl Format {
    fn as_mpv_format(&self) -> &MpvFormat {
        unsafe { mem::transmute::<&Format, &MpvFormat>(self) }
    }

    fn size(&self) -> usize {
        match *self {
            Format::Flag => mem::size_of::<bool>(),
            Format::Int64 => mem::size_of::<libc::int64_t>(),
            Format::Double => mem::size_of::<libc::c_double>(),
            Format::Node => mem::size_of::<MpvNode>(),
            _ => unreachable!(),
        }
    }
}

impl MpvError {
    fn as_val(&self) -> libc::c_int {
        *self as libc::c_int
    }

    #[inline]
    /// Returns a string slice associated with the `MpvError`.
    pub fn error_string(&self) -> &str {
        let raw = unsafe { mpv_error_string(self.as_val()) };
        unsafe { CStr::from_ptr(raw) }.to_str().unwrap()
    }
}

impl MpvFormat {
    fn as_val(self) -> libc::c_int {
        self as libc::c_int
    }
}

impl MpvNode {
    #[inline]
    /// Create a `MpvNode` from a supported value.
    pub fn new<T>(val: T) -> MpvNode
        where T: Into<MpvNode>
    {
        val.into()
    }

    fn get_inner(&self) -> Data {
        // TODO: this.
        unimplemented!();
    }
}

// TODO: impl Into<MpvNode> for types

impl PartialEq for MpvNode {
    fn eq(&self, other: &MpvNode) -> bool {
        self.get_inner() == other.get_inner()
    }
}

/// An mpv instance from which `Client`s can be spawned.
///
/// # Panics
/// Any method on this struct may panic if any argument contains invalid utf-8.
pub struct Parent {
    ctx: *mut MpvHandle,
    initialized: AtomicBool,
    suspension_count: AtomicUsize,
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
pub trait MpvMarker {
    // FIXME: Most of these can go once `Associated Items` lands
    fn initialized(&self) -> bool;
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

impl MpvMarker for Parent {
    fn initialized(&self) -> bool {
        self.initialized.load(Ordering::Acquire)
    }
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

impl<'parent> MpvMarker for Client<'parent> {
    fn initialized(&self) -> bool {
        true
    }
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
    fn drop(&mut self) {
        self.drop_ev_iter_step();
        unsafe {
            mpv_terminate_destroy(self.ctx());
        }
    }
}

impl<'parent> Drop for Client<'parent> {
    fn drop(&mut self) {
        self.drop_ev_iter_step();
        unsafe {
            mpv_detach_destroy(self.ctx());
        }
    }
}

impl<'parent> Parent {
    /// Create a new `Mpv` instance.
    /// To call any method except for `set_option` on this, it has to be initialized first.
    /// The default settings can be probed by running: ```$ mpv --show-profile=libmpv```
    pub fn new(check_events: bool) -> Result<Parent, Error> {
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
            try!(mpv_err((), mpv_request_event(ctx, MpvEventId::TracksChanged, 0)));
            try!(mpv_err((), mpv_request_event(ctx, MpvEventId::TrackSwitched, 0)));
            try!(mpv_err((), mpv_request_event(ctx, MpvEventId::Pause, 0)));
            try!(mpv_err((), mpv_request_event(ctx, MpvEventId::Unpause, 0)));
            try!(mpv_err((),
                         mpv_request_event(ctx, MpvEventId::ScriptInputDispatch, 0)));
            try!(mpv_err((), mpv_request_event(ctx, MpvEventId::MetadataUpdate, 0)));
            try!(mpv_err((), mpv_request_event(ctx, MpvEventId::ChapterChange, 0)));
        }

        let (ev_iter_notification, ev_to_observe, ev_to_observe_properties, ev_observed) =
            if check_events {
                let ev_iter_notification = Box::into_raw(box (Mutex::new(false), Condvar::new()));
                unsafe {
                    mpv_set_wakeup_callback(ctx,
                                            event_callback,
                                            ev_iter_notification as *mut libc::c_void);
                }

                (Some(ev_iter_notification),
                 Some(Mutex::new(Vec::with_capacity(10))),
                 Some(Mutex::new(HashMap::new())),
                 Some(Mutex::new(Vec::with_capacity(10))))
            } else {
                unsafe {
                    // Disable remaining events
                    try!(mpv_err((), mpv_request_event(ctx, MpvEventId::LogMessage, 0)));
                    try!(mpv_err((), mpv_request_event(ctx, MpvEventId::GetPropertyReply, 0)));
                    try!(mpv_err((), mpv_request_event(ctx, MpvEventId::SetPropertyReply, 0)));
                    try!(mpv_err((), mpv_request_event(ctx, MpvEventId::CommandReply, 0)));
                    try!(mpv_err((), mpv_request_event(ctx, MpvEventId::StartFile, 0)));
                    try!(mpv_err((), mpv_request_event(ctx, MpvEventId::EndFile, 0)));
                    try!(mpv_err((), mpv_request_event(ctx, MpvEventId::FileLoaded, 0)));
                    try!(mpv_err((), mpv_request_event(ctx, MpvEventId::Idle, 0)));
                    try!(mpv_err((), mpv_request_event(ctx, MpvEventId::ClientMessage, 0)));
                    try!(mpv_err((), mpv_request_event(ctx, MpvEventId::VideoReconfig, 0)));
                    try!(mpv_err((), mpv_request_event(ctx, MpvEventId::AudioReconfig, 0)));
                    try!(mpv_err((), mpv_request_event(ctx, MpvEventId::Seek, 0)));
                    try!(mpv_err((), mpv_request_event(ctx, MpvEventId::PlaybackRestart, 0)));
                    try!(mpv_err((), mpv_request_event(ctx, MpvEventId::PropertyChange, 0)));
                    try!(mpv_err((), mpv_request_event(ctx, MpvEventId::QueueOverflow, 0)));
                }

                (None, None, None, None)
            };

        Ok(Parent {
            ctx: ctx,
            initialized: AtomicBool::new(false),
            suspension_count: AtomicUsize::new(0),
            check_events: check_events,
            ev_iter_notification: ev_iter_notification,
            ev_to_observe: ev_to_observe,
            ev_to_observe_properties: ev_to_observe_properties,
            ev_observed: ev_observed,
        })
    }

    /// Create a client with `name`, that is connected to the core of `self`, but has an own queue
    /// for API events and such.
    pub fn new_client(&self, name: &str, check_events: bool) -> Result<Client, Error> {
        if self.initialized() {
            let ctx = unsafe {
                let name = CString::new(name).unwrap();
                mpv_create_client(self.ctx(), name.as_ptr())
            };
            unsafe {
                // Disable deprecated events.
                try!(mpv_err((), mpv_request_event(ctx, MpvEventId::TracksChanged, 0)));
                try!(mpv_err((), mpv_request_event(ctx, MpvEventId::TrackSwitched, 0)));
                try!(mpv_err((), mpv_request_event(ctx, MpvEventId::Pause, 0)));
                try!(mpv_err((), mpv_request_event(ctx, MpvEventId::Unpause, 0)));
                try!(mpv_err((),
                             mpv_request_event(ctx, MpvEventId::ScriptInputDispatch, 0)));
                try!(mpv_err((), mpv_request_event(ctx, MpvEventId::MetadataUpdate, 0)));
                try!(mpv_err((), mpv_request_event(ctx, MpvEventId::ChapterChange, 0)));
            }
            let (ev_iter_notification, ev_to_observe, ev_to_observe_properties, ev_observed) =
                if check_events {
                    let ev_iter_notification = Box::into_raw(box (Mutex::new(false),
                                                                  Condvar::new()));
                    unsafe {
                        mpv_set_wakeup_callback(ctx,
                                            event_callback,
                                            ev_iter_notification as *mut libc::c_void);
                    }

                    (Some(ev_iter_notification),
                     Some(Mutex::new(Vec::with_capacity(10))),
                     Some(Mutex::new(HashMap::new())),
                     Some(Mutex::new(Vec::with_capacity(10))))
                } else {
                    unsafe {
                        // Disable remaining events
                        try!(mpv_err((), mpv_request_event(ctx, MpvEventId::LogMessage, 0)));
                        try!(mpv_err((), mpv_request_event(ctx, MpvEventId::GetPropertyReply, 0)));
                        try!(mpv_err((), mpv_request_event(ctx, MpvEventId::SetPropertyReply, 0)));
                        try!(mpv_err((), mpv_request_event(ctx, MpvEventId::CommandReply, 0)));
                        try!(mpv_err((), mpv_request_event(ctx, MpvEventId::StartFile, 0)));
                        try!(mpv_err((), mpv_request_event(ctx, MpvEventId::EndFile, 0)));
                        try!(mpv_err((), mpv_request_event(ctx, MpvEventId::FileLoaded, 0)));
                        try!(mpv_err((), mpv_request_event(ctx, MpvEventId::Idle, 0)));
                        try!(mpv_err((), mpv_request_event(ctx, MpvEventId::ClientMessage, 0)));
                        try!(mpv_err((), mpv_request_event(ctx, MpvEventId::VideoReconfig, 0)));
                        try!(mpv_err((), mpv_request_event(ctx, MpvEventId::AudioReconfig, 0)));
                        try!(mpv_err((), mpv_request_event(ctx, MpvEventId::Seek, 0)));
                        try!(mpv_err((), mpv_request_event(ctx, MpvEventId::PlaybackRestart, 0)));
                        try!(mpv_err((), mpv_request_event(ctx, MpvEventId::PropertyChange, 0)));
                        try!(mpv_err((), mpv_request_event(ctx, MpvEventId::QueueOverflow, 0)));
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
        } else {
            Err(Error::Uninitialized)
        }
    }

    #[cold]
    /// Initialize the mpv core.
    pub fn init(&self) -> Result<(), Error> {
        if self.initialized() {
            Err(Error::AlreadyInitialized)
        } else {
            self.initialized.store(true, Ordering::Release);
            let err = unsafe { mpv_initialize(self.ctx()) };
            mpv_err((), err)
        }
    }

    /// Set an option. This only works before core initialization.
    pub fn set_option(&self, opt: &mut Property) -> Result<(), Error> {
        if self.initialized() {
            return Err(Error::AlreadyInitialized);
        }

        let name = CString::new(&opt.name[..]).unwrap().into_raw();
        let format = opt.data.format().as_val();
        let ret = match opt.data {
            Data::OsdString(_) => Err(Error::OsdStringWrite),
            Data::String(ref v) => {
                let data = CString::new(v.as_bytes()).unwrap().into_raw();

                let ret = mpv_err((), unsafe {
                    mpv_set_option(self.ctx(),
                                   name,
                                   format,
                                   data as *mut libc::c_void)
                });
                unsafe {
                    CString::from_raw(data);
                };
                ret
            }
            _ => {
                let data = &mut opt.data;
                let data = data_ptr!(data);

                mpv_err((),
                        unsafe { mpv_set_option(self.ctx(), name, format, data) })
            }
        };
        unsafe { CString::from_raw(name) };
        ret
    }

    #[cold]
    /// Load a configuration file into the `Mpv` instance.
    /// The path has to be absolute.
    /// This should not be done during runtime.
    /// This overrides previously set options and properties.
    pub fn load_config(&self, path: &Path) -> Result<(), Error> {
        if path.is_relative() {
            Err(Error::ExpectedAbsolute)
        } else if path.is_dir() {
            Err(Error::ExpectedFile)
        } else {
            let file = CString::new(path.to_str().unwrap()).unwrap().into_raw();
            let ret = mpv_err((), unsafe { mpv_load_config_file(self.ctx(), file) });
            unsafe { CString::from_raw(file) };
            ret
        }
    }

    /// Suspend the playback thread, or freeze the core. If the core is suspended, only
    /// client API calls will be accepted, ie. input, redrawing etc. will be suspended.
    /// For the thread to resume there has to be one `resume` call for each `suspend` call.
    pub fn suspend(&self) -> Result<(), Error> {
        if self.initialized() {
            self.suspension_count.fetch_add(1, Ordering::AcqRel);
            Ok(unsafe { mpv_suspend(self.ctx()) })
        } else {
            Err(Error::Uninitialized)
        }
    }

    /// See `suspend`.
    pub fn resume(&self) -> Result<(), Error> {
        if self.initialized() {
            if self.suspension_count.load(Ordering::Acquire) == 0 {
                Err(Error::AlreadyResumed)
            } else {
                self.suspension_count.fetch_sub(1, Ordering::AcqRel);
                Ok(unsafe { mpv_resume(self.ctx()) })
            }
        } else {
            Err(Error::Uninitialized)
        }
    }
}

impl<'parent> Client<'parent> {
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
    /// Enable a given `Event`
    fn enable_event(&self, e: &Event) -> Result<(), Error>;
    /// Disable a given `Event`
    fn disable_event(&self, e: &Event) -> Result<(), Error>;
    /// Observe all `Event`s by means of an `EventIter`
    fn observe_all(&self, events: &[Event]) -> Result<EventIter<P>, Error>;
    /// Execute any mpv command.
    ///
    /// # Unsafety
    /// This is marked as unsafe because any arbitrary binary can be executed, and the `quit`
    /// command or similar may break the invariant of drop order.
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
    /// Cycle a given named property, either up or down.
    fn cycle(&self, property: &str, up: &bool) -> Result<(), Error>;
    /// Multiply a given named property by an unsigned factor.
    fn multiply(&self, property: &str, factor: &usize) -> Result<(), Error>;
    /// Pause playblack.
    fn pause(&self) -> Result<(), Error>;
    /// Unpause playblack.
    fn unpause(&self) -> Result<(), Error>;
}

impl<'parent, P> MpvInstance<'parent, P> for P
    where P: MpvMarker + 'parent
{
    /// Enable a given `Event`. Note that any event of `Event` is enabled by default,
    /// except for `Event::Tick`.
    fn enable_event(&self, e: &Event) -> Result<(), Error> {
        if self.check_events() {
            mpv_err((), unsafe { mpv_request_event(self.ctx(), e.as_id(), 1) })
        } else {
            Err(Error::EventsDisabled)
        }
    }

    /// Disable a given `Event`.
    fn disable_event(&self, e: &Event) -> Result<(), Error> {
        if self.check_events() {
            mpv_err((), unsafe { mpv_request_event(self.ctx(), e.as_id(), 0) })
        } else {
            Err(Error::EventsDisabled)
        }
    }

    /// Observe given `Event`s.
    /// Returns an `EventIter`, on which `next` can be called that blocks while waiting for new
    /// `Event`s.
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
                        return Err(Error::AlreadyObserved(box elem.clone()));
                    } else {
                        props.push(v);
                        ids.push(elem.as_id());
                        evs.push(elem.clone());
                        continue;
                    }
                }
                for id in &(*observe) {
                    if elem.as_id() == id.as_id() {
                        return Err(Error::AlreadyObserved(box elem.clone()));
                    }
                }
                ids.push(elem.as_id());
                evs.push(elem.clone());
            }
            observe.extend(evs.clone());

            for elem in props {
                let id = properties.len();
                unsafe {
                    let name = CString::new(elem.name.clone()).unwrap();
                    try!(mpv_err((),
                                 mpv_observe_property(self.ctx(),
                                                      id as libc::uint64_t,
                                                      name.as_ptr(),
                                                      elem.data.format() as libc::c_int)))
                }
                properties.insert(elem.name.clone(), id as libc::uint64_t);
            }

            Ok(EventIter {
                ctx: self.ctx(),
                notification: self.ev_iter_notification().unwrap(),
                all_to_observe: self.ev_to_observe().as_ref().unwrap(),
                all_to_observe_properties: self.ev_to_observe_properties().as_ref().unwrap(),
                local_to_observe: evs,
                all_observed: self.ev_observed().as_ref().unwrap(),
                last_no_associated_ev: false,
                _does_not_outlive: PhantomData::<&Self>,
            })
        } else {
            Err(Error::EventsDisabled)
        }
    }

    /// Send a command to the `Mpv` instance. This uses `mpv_command_string` internally,
    /// so that the syntax is the same as described in the [manual for the input.conf]
    /// (https://mpv.io/manual/master/#list-of-input-commands). It is advised to use the specific
    /// method for each command, because the specific functions may check for
    /// common errors and are generally type checked (enums to specify operations).
    ///
    /// # Safety
    /// This method is unsafe because the player may quit via the quit command, compromising the
    /// memory safety guarantees of this crate.
    unsafe fn command(&self, cmd: &Command) -> Result<(), Error> {
        // Will probably allocate a little too much, but that is fine to avoid reallocation
        let mut args = String::with_capacity(mem::size_of_val(cmd.args));
        for elem in cmd.args {
            args.push_str(&format!(" {}", elem));
        }
        let args = CString::new(format!("{}{}", cmd.name, args)).unwrap();

        mpv_err((), mpv_command_string(self.ctx(), args.as_ptr()))
    }

    #[allow(match_ref_pats)]
    /// Set the value of a property.
    fn set_property(&self, prop: &mut Property) -> Result<(), Error> {
        let format = prop.data.format().as_val();
        let name = CString::new(&prop.name[..]).unwrap().into_raw();
        let ret = match prop.data {
            Data::OsdString(_) => Err(Error::OsdStringWrite),
            Data::String(ref v) => {
                let data = CString::new(v.as_bytes()).unwrap().into_raw();

                let ret = mpv_err((), unsafe {
                    mpv_set_property(self.ctx(),
                                     name,
                                     format,
                                     data as *mut libc::c_void)
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

    /// Get the value of a property.
    fn get_property(&self, name: &str, format: &Format) -> Result<Data, Error> {
        let name = CString::new(name).unwrap();
        let format = format;
        Ok(match *format {
            Format::String | Format::OsdString => {
                let ptr = CString::new("").unwrap().into_raw();

                let err = mpv_err((), unsafe {
                    mpv_get_property(self.ctx(),
                                     name.as_ptr(),
                                     format.as_mpv_format().as_val(),
                                     ptr as *mut libc::c_void)
                });

                let ret = unsafe { CString::from_raw(ptr) };
                if err.is_err() {
                    return Err(err.unwrap_err());
                } else {
                    let data = if cfg!(windows) {
                        // Mpv claims that all strings returned on windows are UTF-8.
                        ret.to_str().unwrap().to_owned()
                    } else {
                        let bytes = ret.as_bytes();

                        println!("!!!!_DANGER_ZONE_!!!!");
                        // It should be this
                        println!("ref: {:?}", "トゥッティ！".as_bytes());
                        // But we got this
                        println!("got: {:?}", bytes);
                        // Which is this in utf-8
                        println!("ldc: {}", String::from_utf8_lossy(bytes).into_owned());
                        // This is what the OsString is capable of (protip: nothing)
                        use std::ffi::OsStr;
                        use std::os::unix::ffi::OsStrExt;
                        println!("OsS: {:?}", OsStr::from_bytes(bytes));

                        let tmp = encoding::decode(bytes,
                                                   encoding::DecoderTrap::Strict,
                                                   encoding::all::ASCII)
                                      .0
                                      .or_else(|_| {
                                          Err(Error::UnsupportedEncoding(Vec::from(bytes)))
                                      });

                        // And this in the guessed encoding
                        println!("gue: {:?}", tmp);
                        tmp.unwrap()
                    };

                    match *format {
                        Format::String => Data::String(data),
                        Format::OsdString => Data::OsdString(data),
                        _ => unreachable!(),
                    }
                }
            }
            _ => {
                let ptr = unsafe {
                    libc::malloc(format.size() as libc::size_t) as *mut libc::c_void
                };

                let err = mpv_err((), unsafe {
                    mpv_get_property(self.ctx(),
                                     name.as_ptr(),
                                     format.as_mpv_format().as_val(),
                                     ptr)
                });

                if err.is_err() {
                    return Err(err.unwrap_err());
                } else {
                    Data::from_raw(*format.as_mpv_format(), ptr)
                }
            }
        })
    }

    // --- Convenience command functions ---
    //


    /// Seek to a position as defined by `Seek`.
    fn seek(&self, seek: &Seek) -> Result<(), Error> {
        match *seek {
            Seek::RelativeForward(d) => unsafe {
                self.command(&Command::new("seek",
                                           &[format!("{}", d.as_secs()), "relative".into()]))

            },
            Seek::RelativeBackward(d) => unsafe {
                self.command(&Command::new("seek",
                                           &[format!("-{}", d.as_secs()), "relative".into()]))
            },
            Seek::Absolute(d) => unsafe {
                self.command(&Command::new("seek",
                                           &[format!("{}", d.as_secs()), "absolute".into()]))
            },
            Seek::RelativePercent(p) => {
                if p > 100 {
                    // This is actually allowed in libmpv (seek to end),
                    // but it's confusing and may be an indicator of bugs.
                    Err(Error::OutOfBounds)
                } else {
                    unsafe {
                        println!("{:?}",
                                 Command::new("seek",
                                              &[format!("{}", p), "relative-percent".into()]));
                        self.command(&Command::new("seek",
                                                   &[format!("{}", p), "relative-percent".into()]))
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
                                                   &[format!("{}", p), "absolute-percent".into()]))
                    }
                }
            }
            Seek::Revert => unsafe { self.command(&Command::new("revert-seek", &[])) },
            Seek::RevertMark => unsafe {
                self.command(&Command::new("revert-seek", &["mark".into()]))
            },
            Seek::Frame => unsafe { self.command(&Command::new("frame-step", &[])) },
            Seek::FrameBack => unsafe { self.command(&Command::new("frame-back-step", &[])) },
        }
    }

    /// Take a screenshot as defined by `Screenshot`.
    fn screenshot(&self, st: &Screenshot) -> Result<(), Error> {
        match *st {
            Screenshot::Subtitles => unsafe {
                self.command(&Command::new("screenshot", &["subtitles".into()]))
            },
            Screenshot::SubtitlesFile(ref p) => unsafe {
                self.command(&Command::new("screenshot",
                                           &[p.to_str().unwrap().into(), "subtitles".into()]))
            },
            Screenshot::Video => unsafe {
                self.command(&Command::new("screenshot", &["video".into()]))
            },
            Screenshot::VideoFile(ref p) => unsafe {
                self.command(&Command::new("screenshot",
                                           &[p.to_str().unwrap().into(), "video".into()]))
            },
            Screenshot::Window => unsafe {
                self.command(&Command::new("screenshot", &["window".into()]))
            },
            Screenshot::WindowFile(ref p) => unsafe {
                self.command(&Command::new("screenshot",
                                           &[p.to_str().unwrap().into(), "window".into()]))
            },
        }
    }

    /// Execute an operation on the playlist as defined by `PlaylistOp`
    fn playlist(&self, op: &PlaylistOp) -> Result<(), Error> {
        match *op {
            PlaylistOp::NextWeak => unsafe {
                self.command(&Command::new("playlist-next", &["weak".into()]))
            },
            PlaylistOp::NextForce => unsafe {
                self.command(&Command::new("playlist-next", &["force".into()]))
            },
            PlaylistOp::PreviousWeak => unsafe {
                self.command(&Command::new("playlist-previous", &["weak".into()]))
            },
            PlaylistOp::PreviousForce => unsafe {
                self.command(&Command::new("playlist-previous", &["force".into()]))
            },
            PlaylistOp::LoadlistReplace(p) => unsafe {
                self.command(&Command::new("loadlist",
                                           &[format!("\"{}\"", p.to_str().unwrap()),
                                             "replace".into()]))
            },
            PlaylistOp::LoadlistAppend(p) => unsafe {
                self.command(&Command::new("loadlist",
                                           &[format!("\"{}\"", p.to_str().unwrap()),
                                             "append".into()]))
            },
            PlaylistOp::Clear => unsafe { self.command(&Command::new("playlist-clear", &[])) },
            PlaylistOp::RemoveCurrent => unsafe {
                self.command(&Command::new("playlist-remove", &["current".into()]))
            },
            PlaylistOp::RemoveIndex(i) => unsafe {
                self.command(&Command::new("playlist-remove", &[format!("{}", i)]))
            },
            PlaylistOp::Move((old, new)) => unsafe {
                self.command(&Command::new("playlist-move",
                                           &[format!("{}", new), format!("{}", old)]))
            },
            PlaylistOp::Shuffle => unsafe { self.command(&Command::new("playlist-shuffle", &[])) },
            PlaylistOp::Loadfiles(lfiles) => {
                for (i, elem) in lfiles.iter().enumerate() {
                    let ret = unsafe {
                        self.command(&Command {
                            name: "loadfile",
                            args: &match elem.options {
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
                            },
                        })
                    };
                    if ret.is_err() {
                        return Err(Error::Loadfiles((i, box ret.unwrap_err())));
                    }
                }
                Ok(())
            }
        }
    }

    /// Execute an operation as defined by `SubOp`.
    fn subtitle(&self, op: &SubOp) -> Result<(), Error> {
        match *op {
            SubOp::AddSelect(p, t, l) => unsafe {
                self.command(&Command::new("sub-add",
                                           &[format!("\"{}\"", p.to_str().unwrap()),
                                             format!("select{}{}",
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
                                           &[format!("\"{}\"", p.to_str().unwrap()),
                                             format!("auto{}{}",
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
                self.command(&Command::new("sub-add",
                                           &[format!("\"{}\"", p.to_str().unwrap()),
                                             format!("cached{}{}",
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
            SubOp::Remove(i) => unsafe {
                self.command(&Command::new("sub-remove",
                                           &if i.is_some() {
                                               vec![format!("{}", i.unwrap())]
                                           } else {
                                               vec![]
                                           }))
            },
            SubOp::Reload(i) => unsafe {
                self.command(&Command::new("sub-reload",
                                           &if i.is_some() {
                                               vec![format!("{}", i.unwrap())]
                                           } else {
                                               vec![]
                                           }))
            },
            SubOp::Step(i) => unsafe {
                self.command(&Command::new("sub-step", &[format!("{}", i)]))
            },
            SubOp::SeekForward => unsafe { self.command(&Command::new("sub-seek", &["1".into()])) },
            SubOp::SeekBackward => unsafe {
                self.command(&Command::new("sub-seek", &["-1".into()]))
            },
        }
    }

    /// Cycle through a given property. `up` specifies direction. On
    /// overflow, set the property back to the minimum, on underflow set it to the maximum.
    fn cycle(&self, property: &str, up: &bool) -> Result<(), Error> {
        unsafe {
            self.command(&Command::new("cycle",
                                       &[property.into(),
                                         if *up {
                                             "up"
                                         } else {
                                             "down"
                                         }
                                         .into()]))
        }
    }

    /// Multiply any property with any positive factor.
    fn multiply(&self, property: &str, factor: &usize) -> Result<(), Error> {
        unsafe {
            self.command(&Command::new("multiply", &[property.into(), format!("{}", factor)]))
        }
    }

    // --- Convenience property functions ---
    //


    /// Pause playback at runtime.
    fn pause(&self) -> Result<(), Error> {
        self.set_property(&mut Property::new("pause", Data::Flag(true)))
    }

    /// Unpause playback at runtime.
    fn unpause(&self) -> Result<(), Error> {
        self.set_property(&mut Property::new("pause", Data::Flag(false)))
    }
}
