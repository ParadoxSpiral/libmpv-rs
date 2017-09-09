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

use raw::*;

use ::*;
use super::super::*;

use std::iter::Map;
use std::slice;
use std::slice::Iter;
use std::os::raw as ctype;

// TODO: Clean this up, as a lot is duplicated/copied from elsewhere, use cstr macro

#[derive(Debug, Clone, PartialEq)]
#[allow(missing_docs)]
/// Data that is returned by both `GetPropertyReply` and `PropertyChange` events.
pub enum PropertyData<'a> {
    Str(&'a str),
    OsdStr(&'a str),
    Flag(bool),
    Int64(i64),
    Double(ctype::c_double),
}

impl<'a> PropertyData<'a> {
    fn from_raw(format: mpv_format, ptr: *mut ctype::c_void) -> Result<PropertyData<'a>> {
        debug_assert!(!ptr.is_null());
        match format {
            mpv_format::MPV_FORMAT_FLAG => Ok(PropertyData::Flag(unsafe { *(ptr as *mut bool) })),
            mpv_format::MPV_FORMAT_STRING => {
                let char_ptr = unsafe { *(ptr as *mut *mut ctype::c_char) };
                Ok(PropertyData::Str(
                    mpv_cstr_to_str!(unsafe { CStr::from_ptr(char_ptr) })?,
                ))
            }
            mpv_format::MPV_FORMAT_OSD_STRING => {
                let char_ptr = unsafe { *(ptr as *mut *mut ctype::c_char) };
                Ok(PropertyData::OsdStr(
                    mpv_cstr_to_str!(unsafe { CStr::from_ptr(char_ptr) })?,
                ))
            }
            mpv_format::MPV_FORMAT_DOUBLE => {
                Ok(PropertyData::Double(unsafe { *(ptr as *mut f64) }))
            }
            mpv_format::MPV_FORMAT_INT64 => Ok(PropertyData::Int64(unsafe { *(ptr as *mut i64) })),
            mpv_format::MPV_FORMAT_NONE => unreachable!(),
            _ => unimplemented!(),
        }
    }
}

#[derive(Clone, Debug)]
/// Wrapper around an `Iterator` that yields the `str` of `Event::ClientMessage`
pub struct MessageIter<'a>(
    Map<Iter<'a, *const i8>, fn(&'a *const i8) -> Result<&'a str>>,
    usize,
);

impl<'a> Iterator for MessageIter<'a> {
    type Item = Result<&'a str>;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.1, Some(self.1))
    }
}

impl<'a> ExactSizeIterator for MessageIter<'a> {}

impl<'a> PartialEq for MessageIter<'a> {
    fn eq(&self, rhs: &Self) -> bool {
        if self.1 == rhs.len() {
            // TODO: Any way to avoid clone?
            self.0.clone().eq(rhs.0.clone())
        } else {
            false
        }
    }
}

#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq)]
pub enum Event<'a> {
    /// Received when the player is shutting down
    Shutdown,
    /// *Has not been tested*, received when explicitly asked to MPV
    LogMessage {
        prefix: &'a str,
        level: &'a str,
        text: &'a str,
        log_level: LogLevel,
    },
    /// Received when using get_property_async
    GetPropertyReply {
        name: &'a str,
        result: PropertyData<'a>,
        reply_userdata: u64,
    },
    /// Received when using set_property_async
    SetPropertyReply(u64),
    /// Received when using command_async
    CommandReply(u64),
    /// Event received when a new file is playing
    StartFile,
    /// Event received when the file being played currently has stopped, for an error or not
    EndFile(EndFileReason),
    /// Event received when a file has been *loaded*, but has not been started
    FileLoaded,
    TracksChanged,
    /// Deprecated
    TrackSwitched,
    /// Received when the player has no more files to play and is in an idle state
    Idle,
    /// The player paused playback
    Pause,
    /// The player started playback again
    Unpause,
    Tick,
    ClientMessage(MessageIter<'a>),
    VideoReconfig,
    AudioReconfig,
    MetadataUpdate,
    /// The player changed current position
    Seek,
    PlaybackRestart,
    /// Received when used with observe_property
    PropertyChange {
        name: &'a str,
        change: PropertyData<'a>,
        reply_userdata: u64,
    },
    ChapterChange,
    /// Received when the Event Queue is full
    QueueOverflow,
    /// Unused event
    Unused,
}

impl Mpv {
    /// Wait for `timeout` seconds for an `Event`. Passing `0` as `timeout` will poll.
    /// For more information, as always, see the mpv-sys docs of `mpv_wait_event`.
    ///
    /// Returns `Some(Err(...))` if there was invalid utf-8, or if either an
    /// `MPV_EVENT_GET_PROPERTY_REPLY`, `MPV_EVENT_SET_PROPERTY_REPLY`, `MPV_EVENT_COMMAND_REPLY`,
    /// or `MPV_EVENT_PROPERTY_CHANGE` event failed, or if `MPV_EVENT_END_FILE` reported an error.
    ///
    /// # Unsafety
    /// Calling this function simultaneosly multiple times may result in data races. Internally,
    /// `EventIter` also uses `wait_event`. Do not combine the two.
    pub unsafe fn wait_event(&self, timeout: f64) -> Option<Result<Event>> {
        let event = &*mpv_wait_event(self.ctx, timeout);

        match event.event_id {
            mpv_event_id::MPV_EVENT_NONE => None,
            mpv_event_id::MPV_EVENT_SHUTDOWN => Some(Ok(Event::Shutdown)),
            mpv_event_id::MPV_EVENT_LOG_MESSAGE => {
                let log_message = *(event.data as *mut mpv_event_log_message);
                // TODO: Can be made prettier once Carrier stabilized
                let prefix = match mpv_cstr_to_str!(CStr::from_ptr(log_message.prefix)) {
                    Err(e) => return Some(Err(e)),
                    Ok(v) => v,
                };
                let level = match mpv_cstr_to_str!(CStr::from_ptr(log_message.level)) {
                    Err(e) => return Some(Err(e)),
                    Ok(v) => v,
                };
                let text = match mpv_cstr_to_str!(CStr::from_ptr(log_message.text)) {
                    Err(e) => return Some(Err(e)),
                    Ok(v) => v,
                };
                Some(Ok(Event::LogMessage {
                    prefix: prefix,
                    level: level,
                    text: text,
                    log_level: log_message.log_level,
                }))
            }
            mpv_event_id::MPV_EVENT_GET_PROPERTY_REPLY => {
                let property = *(event.data as *mut mpv_event_property);
                let name = match mpv_cstr_to_str!(CStr::from_ptr(property.name)) {
                    Err(e) => return Some(Err(e)),
                    Ok(v) => v,
                };
                let data = if let Err(e) = mpv_err((), event.error) {
                    return Some(Err(e));
                } else {
                    match PropertyData::from_raw(property.format, property.data) {
                        Err(e) => return Some(Err(e)),
                        Ok(v) => v,
                    }
                };
                Some(Ok(Event::GetPropertyReply {
                    name: name,
                    result: data,
                    reply_userdata: event.reply_userdata,
                }))
            }
            mpv_event_id::MPV_EVENT_SET_PROPERTY_REPLY => Some(mpv_err(
                Event::SetPropertyReply(event.reply_userdata),
                event.error,
            )),
            mpv_event_id::MPV_EVENT_COMMAND_REPLY => Some(mpv_err(
                Event::CommandReply(event.reply_userdata),
                event.error,
            )),
            mpv_event_id::MPV_EVENT_START_FILE => Some(Ok(Event::StartFile)),
            mpv_event_id::MPV_EVENT_END_FILE => {
                let end_file = *(event.data as *mut mpv_event_end_file);
                let end_file_reason = mpv_end_file_reason::from(end_file.reason as u32);

                if let Err(e) = mpv_err((), end_file.error) {
                    return Some(Err(e));
                }
                Some(Ok(Event::EndFile(end_file_reason)))
            }
            mpv_event_id::MPV_EVENT_FILE_LOADED => Some(Ok(Event::FileLoaded)),
            mpv_event_id::MPV_EVENT_TRACKS_CHANGED => Some(Ok(Event::TracksChanged)),
            mpv_event_id::MPV_EVENT_TRACK_SWITCHED => Some(Ok(Event::TrackSwitched)),
            mpv_event_id::MPV_EVENT_IDLE => Some(Ok(Event::Idle)),
            mpv_event_id::MPV_EVENT_PAUSE => Some(Ok(Event::Pause)),
            mpv_event_id::MPV_EVENT_UNPAUSE => Some(Ok(Event::Unpause)),
            mpv_event_id::MPV_EVENT_TICK => Some(Ok(Event::Tick)),
            mpv_event_id::MPV_EVENT_SCRIPT_INPUT_DISPATCH => Some(Ok(Event::Unused)),
            mpv_event_id::MPV_EVENT_CLIENT_MESSAGE => {
                let client_message = *(event.data as *mut mpv_event_client_message);

                Some(Ok(Event::ClientMessage(MessageIter(
                    slice::from_raw_parts_mut(client_message.args, client_message.num_args as _)
                        .iter()
                        .map(|msg| mpv_cstr_to_str!(CStr::from_ptr(*msg))),
                    client_message.num_args as _,
                ))))
            }
            mpv_event_id::MPV_EVENT_VIDEO_RECONFIG => Some(Ok(Event::VideoReconfig)),
            mpv_event_id::MPV_EVENT_AUDIO_RECONFIG => Some(Ok(Event::AudioReconfig)),
            mpv_event_id::MPV_EVENT_METADATA_UPDATE => Some(Ok(Event::MetadataUpdate)),
            mpv_event_id::MPV_EVENT_SEEK => Some(Ok(Event::Seek)),
            mpv_event_id::MPV_EVENT_PLAYBACK_RESTART => Some(Ok(Event::PlaybackRestart)),
            mpv_event_id::MPV_EVENT_PROPERTY_CHANGE => {
                let property = *(event.data as *mut mpv_event_property);
                Some(mpv_cstr_to_str!(CStr::from_ptr(property.name)).and_then(
                    |name| {
                        mpv_err((), event.error)?;
                        let data = PropertyData::from_raw(property.format, property.data)?;
                        Ok(Event::PropertyChange {
                            name: name,
                            change: data,
                            reply_userdata: event.reply_userdata,
                        })
                    },
                ))
            }
            mpv_event_id::MPV_EVENT_CHAPTER_CHANGE => Some(Ok(Event::ChapterChange)),
            mpv_event_id::MPV_EVENT_QUEUE_OVERFLOW => Some(Ok(Event::QueueOverflow)),
        }
    }

    /// Observe `name` property for changes. `id` can be used to unobserve this (or many) properties
    /// again.
    ///
    /// Warning: This should probably not be used at the same time as an
    /// `EventIter` observing properties, because the ids will most likely overlap.
    pub fn observe_property(&self, name: &str, format: Format, id: u64) -> Result<()> {
        let name = CString::new(name)?;
        mpv_err((), unsafe {
            mpv_observe_property(self.ctx, id, name.as_ptr(), format.as_mpv_format() as _)
        })
    }

    /// Unobserve any property associated with `id`.
    pub fn unobserve_property(&self, id: u64) -> Result<()> {
        mpv_err((), unsafe { mpv_unobserve_property(self.ctx, id) })
    }
}
