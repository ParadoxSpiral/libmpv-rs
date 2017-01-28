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
use parking_lot::{Condvar, Mutex};

use super::*;
use super::mpv_err;
use super::super::LogLevel;
use super::super::raw::*;

use std::collections::HashMap;
use std::marker::PhantomData;
use std::ffi::CStr;
use std::intrinsics;

pub(crate) unsafe extern "C" fn event_callback(d: *mut libc::c_void) {
    (*(d as *mut (Mutex<bool>, Condvar))).1.notify_one();
}

fn property_from_raw(raw: *mut libc::c_void) -> (String, Data) {
    debug_assert!(!raw.is_null());
    let raw = unsafe { &mut *(raw as *mut MpvEventProperty) };
    (
        unsafe { CStr::from_ptr(raw.name).to_str().unwrap().into() },
        Data::from_raw(raw.format, raw.data)
    )
}

#[doc(hidden)]
#[derive(Clone, Debug, PartialEq)]
/// Designed for internal use.
pub struct InnerEvent {
    event: Event,
    err: Option<Error>,
}

impl InnerEvent {
    fn as_event(&self) -> &Event {
        &self.event
    }
    pub(crate) fn as_id(&self) -> MpvEventId {
        self.event.as_id()
    }
}

#[derive(Clone, Debug, PartialEq)]
#[allow(missing_docs)]
/// An event returned by `EventIter`.
pub enum Event {
    LogMessage(LogMessage),
    StartFile,
    EndFile(Option<EndFile>),
    FileLoaded,
    Idle,
    Tick,
    VideoReconfig,
    AudioReconfig,
    Seek,
    PlaybackRestart,
    PropertyChange((String, Data)),
}

impl Event {
    pub(crate) fn as_id(&self) -> MpvEventId {
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
    // WARNING: This ignores the error value, as it is only used for asynchronous calls
    fn as_event(&self) -> Event {
        debug_assert!(mpv_err((), self.error).is_ok());
        match self.event_id {
            MpvEventId::LogMessage => Event::LogMessage(LogMessage::from_raw(self.data)),
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
            MpvEventId::PropertyChange => Event::PropertyChange(property_from_raw(self.data)),
            _ => unreachable!(),
        }
    }
    fn as_inner_event(&self) -> InnerEvent {
        InnerEvent {
            event: match self.event_id {
                MpvEventId::LogMessage => Event::LogMessage(LogMessage::from_raw(self.data)),
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
                MpvEventId::PropertyChange => Event::PropertyChange(property_from_raw(self.data)),
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

#[derive(Debug)]
/// A blocking `Iterator` over some observed events of an mpv instance.
/// Once the `EventIter` is dropped, it's `Event`s are removed from
/// the "to be observed" queue, therefore new `Event` invocations won't be observed.
pub struct EventIter<'parent, P>
    where P: MpvInstance + 'parent
{
    pub(crate) ctx: *mut MpvHandle,
    pub(crate) first_iteration: bool,
    pub(crate) notification: *mut (Mutex<bool>, Condvar),
    pub(crate) all_to_observe: &'parent Mutex<Vec<Event>>,
    pub(crate) all_to_observe_properties: &'parent Mutex<HashMap<String, libc::uint64_t>>,
    pub(crate) local_to_observe: Vec<Event>,
    pub(crate) all_observed: &'parent Mutex<Vec<InnerEvent>>,
    pub(crate) _does_not_outlive: PhantomData<&'parent P>,
}

impl<'parent, P> Drop for EventIter<'parent, P>
    where P: MpvInstance + 'parent
{
    fn drop(&mut self) {
        let mut all_to_observe = self.all_to_observe.lock();
        let mut all_observed = self.all_observed.lock();
        let mut all_to_observe_properties = self.all_to_observe_properties.lock();

        // Returns true if outer and inner event match, if so, the event is unobserved.
        let mut compare_ev_unobserve = |outer_ev: &Event, inner_ev: &Event| {
            if let Event::PropertyChange(ref outer_prop) = *outer_ev {
                if let Event::PropertyChange(ref inner_prop) = *inner_ev {
                    // `.0` is the name of the property.
                    if outer_prop.0 == inner_prop.0 {
                        unsafe {
                            mpv_unobserve_property(self.ctx, all_to_observe_properties.remove(
                                                                        &outer_prop.0).unwrap());
                        }
                        return true;
                    }
                } else if MpvEventId::LogMessage == outer_ev.as_id() &&
                   outer_ev.as_id() == inner_ev.as_id() {
                    debug_assert!(MpvLogLevel::None.as_str() == "no");
                    let min_level = &*b"no0";
                    mpv_err((),
                            unsafe { mpv_request_log_messages(self.ctx, min_level.as_ptr() as _) })
                        .unwrap();
                        return true;
                }
            } else if outer_ev.as_id() == inner_ev.as_id() {
                unsafe { mpv_request_event(self.ctx, inner_ev.as_id(), 0) };
                return true;
            }
            false
        };

        // This removes all events for which compare_ev_unobserve returns true.
        for outer_ev in &self.local_to_observe {
            all_to_observe.retain(|inner_ev| !compare_ev_unobserve(outer_ev, inner_ev));
            all_observed.retain(|inner_ev| !compare_ev_unobserve(outer_ev, (*inner_ev).as_event()));
        }
    }
}

impl<'parent, P> Iterator for EventIter<'parent, P>
    where P: MpvInstance + 'parent
{
    type Item = Vec<Event>;

    fn next(&mut self) -> Option<Self::Item> {
        // Loop until some events can be returned
        'no_events_anchor: loop {
            let mut observed = self.all_observed.lock();
            if observed.is_empty() && !self.first_iteration {
                drop(observed);
                unsafe { (*self.notification).1.wait(&mut (*self.notification).0.lock()) };
                observed = self.all_observed.lock();
            }

            let mut ret_events = Vec::with_capacity(observed.len());
            if observed.is_empty() || self.first_iteration {
                let all_to_observe = self.all_to_observe.lock();
                let mut last = false;
                'events: loop {
                    let event = unsafe { &*mpv_wait_event(self.ctx, 0f32 as _) };
                    let ev_id = &event.event_id;

                    if unsafe { intrinsics::unlikely(ev_id == &MpvEventId::QueueOverflow) } {
                        // The queue needs to be emptied asap to prevent loss of events
                        // This should happen very rarely, as the queue size is 1k (2016-10-12)
                        break;
                    } else if ev_id == &MpvEventId::None {
                        if last {
                            break;
                        } else {
                            last = true;
                            continue;
                        }
                    }
                    for local_ob_ev in &self.local_to_observe {
                        if ev_id == &local_ob_ev.as_id() {
                            ret_events.push(event.as_event());
                            continue 'events;
                        }
                    }
                    for all_ob_ev in &*all_to_observe {
                        if ev_id == &all_ob_ev.as_id() {
                            observed.push(event.as_inner_event());
                            continue 'events;
                        }
                    }
                }
                if !observed.is_empty() {
                    drop(observed);
                    unsafe { (*self.notification).1.notify_all() };
                }
            } else {
                // Return true where outer_ev == inner_ev, and push inner_ev to ret_events
                let mut compare_ev = |outer_ev: &Event, inner_ev: &InnerEvent| {
                    if let Event::PropertyChange(ref outer_prop) = *outer_ev {
                        if let Event::PropertyChange(ref inner_prop) = *inner_ev.as_event() {
                            if outer_prop.0 == inner_prop.0 {
                                ret_events.push(inner_ev.as_event().clone());
                                return true;
                            }
                        }
                    } else if outer_ev.as_id() == inner_ev.as_id() {
                        ret_events.push(inner_ev.as_event().clone());
                        return true;
                    }
                    false
                };
                // Remove events belonging to this EventIter from observed
                for outer_ev in &self.local_to_observe {
                    observed.retain(|inner_ev| !compare_ev(outer_ev, inner_ev));
                }

                if !observed.is_empty() {
                    drop(observed);
                    unsafe { (*self.notification).1.notify_all() };
                }
            }
            
            self.first_iteration = false;

            if !ret_events.is_empty() {
                ret_events.shrink_to_fit();
                return Some(ret_events);
            } else {
                continue 'no_events_anchor;
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[allow(missing_docs)]
/// The data of an `Event::LogMessage`.
pub struct LogMessage {
    pub prefix: String,
    pub level: LogLevel,
    pub text: String,
    pub log_level: LogLevel,
}

impl LogMessage {
    /// Create an empty `LogMessage` with specified verbosity, useful for observing.
    pub fn new(lvl: LogLevel) -> LogMessage {
        LogMessage {
            prefix: "".into(),
            level: lvl,
            text: "".into(),
            log_level: lvl,
        }
    }

    fn from_raw(raw: *mut libc::c_void) -> LogMessage {
        debug_assert!(!raw.is_null());
        let raw = unsafe { &mut *(raw as *mut MpvEventLogMessage) };
        LogMessage {
            prefix: unsafe { CStr::from_ptr(raw.prefix).to_str().unwrap().into() },
            level: unsafe { MpvLogLevel::from_str(CStr::from_ptr(raw.level).to_str().unwrap()) },
            text: unsafe { CStr::from_ptr(raw.text).to_str().unwrap().into() },
            log_level: raw.log_level,
        }
    }
}

impl MpvLogLevel {
    pub(crate) fn as_str(&self) -> &str {
        match *self {
            MpvLogLevel::None => "no",
            MpvLogLevel::Fatal => "fatal",
            MpvLogLevel::Error => "error",
            MpvLogLevel::Warn => "warn",
            MpvLogLevel::Info => "info",
            MpvLogLevel::V => "v",
            MpvLogLevel::Debug => "debug",
            MpvLogLevel::Trace => "trace",
        }
    }

    #[allow(should_implement_trait)]
    fn from_str(name: &str) -> MpvLogLevel {
        match name {
            "no" => MpvLogLevel::None,
            "fatal" => MpvLogLevel::Fatal,
            "error" => MpvLogLevel::Error,
            "warn" => MpvLogLevel::Warn,
            "info" => MpvLogLevel::Info,
            "v" => MpvLogLevel::V,
            "debug" => MpvLogLevel::Debug,
            "trace" => MpvLogLevel::Trace,
            _ => unreachable!(),
        }
    }
}

impl MpvEventEndFile {
    fn from_raw(raw: *mut libc::c_void) -> MpvEventEndFile {
        debug_assert!(!raw.is_null());
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
