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

use raw::*;

use super::*;
use super::mpv_err;
use super::super::{LogLevel, EndFileReason};

use std::ffi::CStr;
use std::os::raw as ctype;

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

    fn as_id(&self) -> mpv_event_id {
        match *self {
            Event::LogMessage { .. } => mpv_event_id::MPV_EVENT_LOG_MESSAGE,
            Event::StartFile => mpv_event_id::MPV_EVENT_START_FILE,
            Event::EndFile { .. } => mpv_event_id::MPV_EVENT_END_FILE,
            Event::FileLoaded => mpv_event_id::MPV_EVENT_FILE_LOADED,
            Event::Idle => mpv_event_id::MPV_EVENT_IDLE,
            Event::Tick => mpv_event_id::MPV_EVENT_TICK,
            Event::VideoReconfig => mpv_event_id::MPV_EVENT_VIDEO_RECONFIG,
            Event::AudioReconfig => mpv_event_id::MPV_EVENT_AUDIO_RECONFIG,
            Event::Seek => mpv_event_id::MPV_EVENT_SEEK,
            Event::PlaybackRestart => mpv_event_id::MPV_EVENT_PLAYBACK_RESTART,
            Event::PropertyChange(_) => mpv_event_id::MPV_EVENT_PROPERTY_CHANGE,
        }
    }

    fn from_raw(raw: &mpv_event) -> Event {
        debug_assert!(mpv_err((), raw.error).is_ok());
        match raw.event_id {
            mpv_event_id::MPV_EVENT_LOG_MESSAGE => Event::logmessage_from_raw(raw.data),
            mpv_event_id::MPV_EVENT_START_FILE => Event::StartFile,
            mpv_event_id::MPV_EVENT_END_FILE => Event::endfile_from_raw(raw.data),
            mpv_event_id::MPV_EVENT_FILE_LOADED => Event::FileLoaded,
            mpv_event_id::MPV_EVENT_IDLE => Event::Idle,
            mpv_event_id::MPV_EVENT_TICK => Event::Tick,
            mpv_event_id::MPV_EVENT_VIDEO_RECONFIG => Event::VideoReconfig,
            mpv_event_id::MPV_EVENT_AUDIO_RECONFIG => Event::AudioReconfig,
            mpv_event_id::MPV_EVENT_SEEK => Event::Seek,
            mpv_event_id::MPV_EVENT_PLAYBACK_RESTART => Event::PlaybackRestart,
            mpv_event_id::MPV_EVENT_PROPERTY_CHANGE => Event::property_from_raw(raw.data),
            _ => unreachable!(),
        }
    }

    fn endfile_from_raw(raw: *mut ctype::c_void) -> Event {
        debug_assert!(!raw.is_null());
        let raw = unsafe { &mut *(raw as *mut mpv_event_end_file) };

        Event::EndFile {
            reason: mpv_end_file_reason::from(raw.reason as u32),
            error: {
                let err = mpv_err((), raw.error);
                if err.is_err() {
                    Some(err.unwrap_err())
                } else {
                    None
                }
            },
        }
    }

    fn logmessage_from_raw(raw: *mut ctype::c_void) -> Event {
        debug_assert!(!raw.is_null());
        let raw = unsafe { &mut *(raw as *mut mpv_event_log_message) };
        Event::LogMessage {
            prefix: unsafe { CStr::from_ptr(raw.prefix).to_str().unwrap().into() },
            level: raw.log_level,
            text: unsafe { CStr::from_ptr(raw.text).to_str().unwrap().into() },
        }
    }

    fn property_from_raw(raw: *mut ctype::c_void) -> Event {
        debug_assert!(!raw.is_null());
        let raw = unsafe { &mut *(raw as *mut mpv_event_property) };
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
    Int64(i64),
    Double(ctype::c_double),
}

impl PropertyData {
    fn format(&self) -> mpv_format {
        match *self {
            PropertyData::String(_) => mpv_format::MPV_FORMAT_STRING,
            PropertyData::OsdString(_) => mpv_format::MPV_FORMAT_OSD_STRING,
            PropertyData::Flag(_) => mpv_format::MPV_FORMAT_FLAG,
            PropertyData::Int64(_) => mpv_format::MPV_FORMAT_INT64,
            PropertyData::Double(_) => mpv_format::MPV_FORMAT_DOUBLE,
        }
    }

    fn from_raw(fmt: mpv_format, ptr: *mut ctype::c_void) -> PropertyData {
        debug_assert!(!ptr.is_null());
        match fmt {
            mpv_format::MPV_FORMAT_FLAG => PropertyData::Flag(unsafe { *(ptr as *mut i64) } != 0),
            mpv_format::MPV_FORMAT_INT64 => PropertyData::Int64(unsafe { *(ptr as *mut _) }),
            mpv_format::MPV_FORMAT_DOUBLE => PropertyData::Double(unsafe { *(ptr as *mut _) }),
            _ => unreachable!(),
        }
    }
}

// TODO: How to do this more nicely? Coherence doesn't make this easy
fn mpv_log_level_as_str(lvl: mpv_log_level) -> &'static str {
    match lvl {
        mpv_log_level::MPV_LOG_LEVEL_NONE => "no",
        mpv_log_level::MPV_LOG_LEVEL_FATAL => "fatal",
        mpv_log_level::MPV_LOG_LEVEL_ERROR => "error",
        mpv_log_level::MPV_LOG_LEVEL_WARN => "warn",
        mpv_log_level::MPV_LOG_LEVEL_INFO => "info",
        mpv_log_level::MPV_LOG_LEVEL_V => "v",
        mpv_log_level::MPV_LOG_LEVEL_DEBUG => "debug",
        mpv_log_level::MPV_LOG_LEVEL_TRACE => "trace",
    }
}
