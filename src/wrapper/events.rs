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
#[cfg(feature="events")]
use parking_lot::{Condvar, Mutex};

use super::*;
use super::mpv_err;
use super::super::{LogLevel, EndFileReason};
use super::super::raw::*;

#[cfg(feature="events")]
use std::collections::HashMap;
#[cfg(feature="events")]
use std::marker::PhantomData;
use std::ffi::CStr;

#[cfg(feature="events")]
pub(crate) unsafe extern "C" fn event_callback(d: *mut libc::c_void) {
    (*(d as *mut Condvar)).notify_one();
}

#[derive(Debug, Clone)]
#[allow(missing_docs)]
/// Data that is returned by the `PropertyChange` event.
pub enum PropertyData {
    String(String),
    OsdString(String),
    Flag(bool),
    Int64(libc::int64_t),
    Double(libc::c_double),
}

impl PropertyData {
    pub(crate) fn format(&self) -> MpvFormat {
        match *self {
            PropertyData::String(_) => MpvFormat::String,
            PropertyData::OsdString(_) => MpvFormat::OsdString,
            PropertyData::Flag(_) => MpvFormat::Flag,
            PropertyData::Int64(_) => MpvFormat::Int64,
            PropertyData::Double(_) => MpvFormat::Double,
        }
    }

    fn from_raw(fmt: MpvFormat, ptr: *mut libc::c_void) -> PropertyData {
        debug_assert!(!ptr.is_null());
        match fmt {
            MpvFormat::Flag => PropertyData::Flag(unsafe { *(ptr as *mut libc::int64_t) } != 0),
            MpvFormat::Int64 => PropertyData::Int64(unsafe { *(ptr as *mut _) }),
            MpvFormat::Double => PropertyData::Double(unsafe { *(ptr as *mut _) }),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
#[allow(missing_docs)]
/// An event returned by libmpv.
///
/// Equality is implemented as equality between variants, not values.
pub enum Event {
    LogMessage {
        prefix: String,
        level: LogLevel,
        text: String,
    },
    StartFile,
    EndFile {
        reason: EndFileReason,
        error: Option<Error>,
    },
    FileLoaded,
    Idle,
    Tick,
    VideoReconfig,
    AudioReconfig,
    Seek,
    PlaybackRestart,
    PropertyChange((String, PropertyData)),
}

impl PartialEq for Event {
    fn eq(&self, rhs: &Event) -> bool {
        match (self, rhs) {
            (&Event::LogMessage { .. }, &Event::LogMessage { .. }) |
            (&Event::StartFile, &Event::StartFile) |
            (&Event::EndFile { .. }, &Event::EndFile { .. }) |
            (&Event::FileLoaded, &Event::FileLoaded) |
            (&Event::Idle, &Event::Idle) |
            (&Event::Tick, &Event::Tick) |
            (&Event::VideoReconfig, &Event::VideoReconfig) |
            (&Event::AudioReconfig, &Event::AudioReconfig) |
            (&Event::Seek, &Event::Seek) |
            (&Event::PlaybackRestart, &Event::PlaybackRestart) |
            (&Event::PropertyChange(_), &Event::PropertyChange(_)) => true,
            _ => false,
        }
    }
}

impl Event {
    /// Create an empty `Event::LogMessage` with given `LogLevel`.
    /// Use this to observe log messages.
    pub fn empty_logmessage(lvl: LogLevel) -> Event {
        Event::LogMessage {
            prefix: "".into(),
            level: lvl,
            text: "".into(),
        }
    }

    pub(crate) fn as_id(&self) -> MpvEventId {
        match *self {
            Event::LogMessage { .. } => MpvEventId::LogMessage,
            Event::StartFile => MpvEventId::StartFile,
            Event::EndFile { .. } => MpvEventId::EndFile,
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

    fn endfile_from_raw(raw: *mut libc::c_void) -> Event {
        debug_assert!(!raw.is_null());
        let raw = unsafe { &mut *(raw as *mut MpvEventEndFile) };

        Event::EndFile {
            reason: raw.reason,
            error: {
                let err = MpvError::from_i32(raw.error).unwrap();
                if err != MpvError::Success {
                    Some(err.into())
                } else {
                    None
                }
            },
        }
    }

    fn logmessage_from_raw(raw: *mut libc::c_void) -> Event {
        debug_assert!(!raw.is_null());
        let raw = unsafe { &mut *(raw as *mut MpvEventLogMessage) };
        Event::LogMessage {
            prefix: unsafe { CStr::from_ptr(raw.prefix).to_str().unwrap().into() },
            level: raw.log_level,
            text: unsafe { CStr::from_ptr(raw.text).to_str().unwrap().into() },
        }
    }

    fn property_from_raw(raw: *mut libc::c_void) -> Event {
        debug_assert!(!raw.is_null());
        let raw = unsafe { &mut *(raw as *mut MpvEventProperty) };
        Event::PropertyChange((unsafe { CStr::from_ptr(raw.name).to_str().unwrap().into() },
                               PropertyData::from_raw(raw.format, raw.data)))
    }
}

impl MpvEvent {
    // WARNING: This ignores the error value, as it is only used for asynchronous calls
    fn as_owned(&self) -> Event {
        debug_assert!(mpv_err((), self.error).is_ok());
        match self.event_id {
            MpvEventId::LogMessage => Event::logmessage_from_raw(self.data),
            MpvEventId::StartFile => Event::StartFile,
            MpvEventId::EndFile => Event::endfile_from_raw(self.data),
            MpvEventId::FileLoaded => Event::FileLoaded,
            MpvEventId::Idle => Event::Idle,
            MpvEventId::Tick => Event::Tick,
            MpvEventId::VideoReconfig => Event::VideoReconfig,
            MpvEventId::AudioReconfig => Event::AudioReconfig,
            MpvEventId::Seek => Event::Seek,
            MpvEventId::PlaybackRestart => Event::PlaybackRestart,
            MpvEventId::PropertyChange => Event::property_from_raw(self.data),
            _ => unreachable!(),
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
}

#[cfg(feature="events")]
/// A blocking `Iterator` over some observed events of an mpv instance.
/// Once the `EventIter` is dropped, it's `Event`s are removed from
/// the "to be observed" queue, therefore new `Event` invocations won't be observed.
pub struct EventIter<'parent, P>
    where P: MpvInstance + 'parent
{
    pub(crate) ctx: *mut MpvHandle,
    pub(crate) first_iteration: bool,
    pub(crate) notification: &'parent (Mutex<bool>, Condvar),
    pub(crate) all_to_observe: &'parent Mutex<Vec<Event>>,
    pub(crate) all_to_observe_properties: &'parent Mutex<HashMap<String, libc::uint64_t>>,
    pub(crate) local_to_observe: Vec<Event>,
    pub(crate) all_observed: &'parent Mutex<Vec<Event>>,
    pub(crate) _does_not_outlive: PhantomData<&'parent P>,
}

#[cfg(feature="events")]
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
                            mpv_unobserve_property(self.ctx,
                                                   all_to_observe_properties
                                                       .remove(&outer_prop.0)
                                                       .unwrap());
                        }
                        return true;
                    }
                } else if MpvEventId::LogMessage == outer_ev.as_id() && outer_ev == inner_ev {
                    debug_assert_eq!("no", MpvLogLevel::None.as_str());
                    let min_level = &*b"no0";
                    unsafe { mpv_request_log_messages(self.ctx, min_level.as_ptr() as _) };
                    return true;
                }
            } else if outer_ev == inner_ev {
                unsafe { mpv_request_event(self.ctx, inner_ev.as_id(), 0) };
                return true;
            }
            false
        };

        // This removes all events for which compare_ev_unobserve returns true.
        for outer_ev in &self.local_to_observe {
            all_to_observe.retain(|inner_ev| !compare_ev_unobserve(outer_ev, inner_ev));
            all_observed.retain(|inner_ev| !compare_ev_unobserve(outer_ev, inner_ev));
        }
    }
}

#[cfg(feature="events")]
impl<'parent, P> Iterator for EventIter<'parent, P>
    where P: MpvInstance + 'parent
{
    type Item = Vec<Event>;

    fn next(&mut self) -> Option<Self::Item> {
        // Loop until some events can be returned
        loop {
            let mut observed = self.all_observed.lock();
            if observed.is_empty() && !self.first_iteration {
                drop(observed);
                self.notification.1.wait(&mut self.notification.0.lock());
                observed = self.all_observed.lock();
            }

            let mut ret_events = Vec::with_capacity(observed.len());
            if observed.is_empty() || self.first_iteration {
                let all_to_observe = self.all_to_observe.lock();
                let mut last = false;
                'events: loop {
                    let event = unsafe { &*mpv_wait_event(self.ctx, 0f32 as _) };
                    let ev_id = event.event_id;

                    if ev_id == MpvEventId::QueueOverflow {
                        // The queue needs to be emptied asap to prevent loss of events
                        // This should happen very rarely, as the queue size is 1k (2016-10-12)
                        break;
                    } else if ev_id == MpvEventId::None {
                        if last {
                            break;
                        } else {
                            last = true;
                            continue;
                        }
                    }
                    for local_ob_ev in &self.local_to_observe {
                        if ev_id == local_ob_ev.as_id() {
                            ret_events.push(event.as_owned());
                            continue 'events;
                        }
                    }
                    for all_ob_ev in &*all_to_observe {
                        if ev_id == all_ob_ev.as_id() {
                            observed.push(event.as_owned());
                            continue 'events;
                        }
                    }
                }
                if !observed.is_empty() {
                    drop(observed);
                    self.notification.1.notify_all();
                }
            } else {
                // Return true where outer_ev == inner_ev, and push inner_ev to ret_events
                let mut compare_ev = |outer_ev: &Event, inner_ev: &Event| {
                    if let Event::PropertyChange(ref outer_prop) = *outer_ev {
                        if let Event::PropertyChange(ref inner_prop) = *inner_ev {
                            if outer_prop.0 == inner_prop.0 {
                                ret_events.push(inner_ev.clone());
                                return true;
                            }
                        }
                    } else if outer_ev == inner_ev {
                        ret_events.push(inner_ev.clone());
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
                    self.notification.1.notify_all();
                }
            }

            self.first_iteration = false;

            if !ret_events.is_empty() {
                ret_events.shrink_to_fit();
                return Some(ret_events);
            }
        }
    }
}
