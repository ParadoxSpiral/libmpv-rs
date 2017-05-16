// Copyright (C) 2016  ParadoxSpiral
//
// This file is part of mpv-sys.
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

#![cfg_attr(feature="nightly", feature(untagged_unions))]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

impl mpv_error {
    #[inline]
    /// Returns the associated error string.
    pub fn error_string(&self) -> &str {
        let raw = unsafe { mpv_error_string(*self as _) };
        unsafe { ::std::ffi::CStr::from_ptr(raw) }.to_str().unwrap()
    }
}

impl ::std::fmt::Display for mpv_error {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl ::std::error::Error for mpv_error {
    fn description(&self) -> &str {
        "An API call did not execute successfully."
    }
}

impl From<i32> for mpv_error {
    fn from(other: i32) -> mpv_error {
        match other {
            0 => mpv_error::MPV_ERROR_SUCCESS,
            -1 => mpv_error::MPV_ERROR_EVENT_QUEUE_FULL,
            -2 => mpv_error::MPV_ERROR_NOMEM,
            -3 => mpv_error::MPV_ERROR_UNINITIALIZED,
            -4 => mpv_error::MPV_ERROR_INVALID_PARAMETER,
            -5 => mpv_error::MPV_ERROR_OPTION_NOT_FOUND,
            -6 => mpv_error::MPV_ERROR_OPTION_FORMAT,
            -7 => mpv_error::MPV_ERROR_OPTION_ERROR,
            -8 => mpv_error::MPV_ERROR_PROPERTY_NOT_FOUND,
            -9 => mpv_error::MPV_ERROR_PROPERTY_FORMAT,
            -10 => mpv_error::MPV_ERROR_PROPERTY_UNAVAILABLE,
            -11 => mpv_error::MPV_ERROR_PROPERTY_ERROR,
            -12 => mpv_error::MPV_ERROR_COMMAND,
            -13 => mpv_error::MPV_ERROR_LOADING_FAILED,
            -14 => mpv_error::MPV_ERROR_AO_INIT_FAILED,
            -15 => mpv_error::MPV_ERROR_VO_INIT_FAILED,
            -16 => mpv_error::MPV_ERROR_NOTHING_TO_PLAY,
            -17 => mpv_error::MPV_ERROR_UNKNOWN_FORMAT,
            -18 => mpv_error::MPV_ERROR_UNSUPPORTED,
            -19 => mpv_error::MPV_ERROR_NOT_IMPLEMENTED,
            -20 => mpv_error::MPV_ERROR_GENERIC,
            _ => unreachable!(),
        }
    }
}

impl From<u32> for mpv_event_id {
    fn from(other: u32) -> mpv_event_id {
        match other {
            0 => mpv_event_id::MPV_EVENT_NONE,
            1 => mpv_event_id::MPV_EVENT_SHUTDOWN,
            2 => mpv_event_id::MPV_EVENT_LOG_MESSAGE,
            3 => mpv_event_id::MPV_EVENT_GET_PROPERTY_REPLY,
            4 => mpv_event_id::MPV_EVENT_SET_PROPERTY_REPLY,
            5 => mpv_event_id::MPV_EVENT_COMMAND_REPLY,
            6 => mpv_event_id::MPV_EVENT_START_FILE,
            7 => mpv_event_id::MPV_EVENT_END_FILE,
            8 => mpv_event_id::MPV_EVENT_FILE_LOADED,
            9 => mpv_event_id::MPV_EVENT_TRACKS_CHANGED,
            10 => mpv_event_id::MPV_EVENT_TRACK_SWITCHED,
            11 => mpv_event_id::MPV_EVENT_IDLE,
            12 => mpv_event_id::MPV_EVENT_PAUSE,
            13 => mpv_event_id::MPV_EVENT_UNPAUSE,
            14 => mpv_event_id::MPV_EVENT_TICK,
            15 => mpv_event_id::MPV_EVENT_SCRIPT_INPUT_DISPATCH,
            16 => mpv_event_id::MPV_EVENT_CLIENT_MESSAGE,
            17 => mpv_event_id::MPV_EVENT_VIDEO_RECONFIG,
            18 => mpv_event_id::MPV_EVENT_AUDIO_RECONFIG,
            19 => mpv_event_id::MPV_EVENT_METADATA_UPDATE,
            20 => mpv_event_id::MPV_EVENT_SEEK,
            21 => mpv_event_id::MPV_EVENT_PLAYBACK_RESTART,
            22 => mpv_event_id::MPV_EVENT_PROPERTY_CHANGE,
            23 => mpv_event_id::MPV_EVENT_CHAPTER_CHANGE,
            24 => mpv_event_id::MPV_EVENT_QUEUE_OVERFLOW,
            _ => unreachable!(),
        }
    }
}

impl From<u32> for mpv_end_file_reason {
    fn from(other: u32) -> mpv_end_file_reason {
        match other {
            0 => mpv_end_file_reason::MPV_END_FILE_REASON_EOF,
            2 => mpv_end_file_reason::MPV_END_FILE_REASON_STOP,
            3 => mpv_end_file_reason::MPV_END_FILE_REASON_QUIT,
            4 => mpv_end_file_reason::MPV_END_FILE_REASON_ERROR,
            5 => mpv_end_file_reason::MPV_END_FILE_REASON_REDIRECT,
            _ => unreachable!(),
        }
    }
}
