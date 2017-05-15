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

#[cfg(feature="events_complex")]
/// TODO
pub mod events_complex;
#[cfg(feature="events_simple")]
/// TODO
pub mod events_simple;

use libc;

use super::*;
use super::mpv_err;
use super::super::{LogLevel, EndFileReason};
use super::super::raw::*;

use std::ffi::CStr;

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

    fn as_id(&self) -> MpvEventId {
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
    fn format(&self) -> MpvFormat {
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
    fn as_str(&self) -> &str {
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
