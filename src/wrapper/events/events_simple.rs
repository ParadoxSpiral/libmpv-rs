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

use raw;

use ::*;
use super::mpv_event_id;
use super::super::*;

use std::iter::Map;
use std::slice;
use std::slice::Iter;
use std::os::raw as ctype;

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
    fn from_raw(format: MpvFormat, ptr: *mut ctype::c_void) -> Result<PropertyData<'a>> {
        assert!(!ptr.is_null());
        match format {
            mpv_format::Flag => Ok(PropertyData::Flag(unsafe { *(ptr as *mut bool) })),
            mpv_format::String => {
                let char_ptr = unsafe { *(ptr as *mut *mut ctype::c_char) };
                Ok(PropertyData::Str(mpv_cstr_to_str!(unsafe {
                    CStr::from_ptr(char_ptr)
                })?))
            }
            mpv_format::OsdString => {
                let char_ptr = unsafe { *(ptr as *mut *mut ctype::c_char) };
                Ok(PropertyData::OsdStr(mpv_cstr_to_str!(unsafe {
                    CStr::from_ptr(char_ptr)
                })?))
            }
            mpv_format::Double => Ok(PropertyData::Double(unsafe { *(ptr as *mut f64) })),
            mpv_format::Int64 => Ok(PropertyData::Int64(unsafe { *(ptr as *mut i64) })),
            mpv_format::None => unreachable!(),
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
    /// Received when the player has no more files to play and is in an idle state
    Idle,
    Unpause,
    Tick,
    ClientMessage(MessageIter<'a>),
    VideoReconfig,
    AudioReconfig,
    /// The player changed current position
    Seek,
    PlaybackRestart,
    /// Received when used with observe_property
    PropertyChange {
        name: &'a str,
        change: PropertyData<'a>,
        reply_userdata: u64,
    },
    /// Received when the Event Queue is full
    QueueOverflow,
    /// A deprecated or unknown event
    Deprecated(super::EventId),
}

impl Mpv {
    /// Wait for `timeout` seconds for an `Event`. Passing `0` as `timeout` will poll.
    /// For more information, as always, see the mpv-sys docs of `mpv_wait_event`.
    ///
    /// Returns `Some(Err(...))` if there was invalid utf-8, or if either an
    /// `MPV_EVENT_GET_PROPERTY_REPLY`, `MPV_EVENT_SET_PROPERTY_REPLY`, `MPV_EVENT_COMMAND_REPLY`,
    /// or `MPV_EVENT_PROPERTY_CHANGE` event failed, or if `MPV_EVENT_END_FILE` reported an error.
    ///
    /// # Safety
    /// An internally used API function is not thread-safe, thus using this method from multiple
    /// threads is UB.
    pub unsafe fn wait_event(&self, timeout: f64) -> Option<Result<Event>> {
        let event = &*raw::mpv_wait_event(self.ctx.as_ptr(), timeout);
        if event.event_id != mpv_event_id::None {
            if let Err(e) = mpv_err((), event.error) {
                return Some(Err(e));
            }
        }

        match event.event_id {
            mpv_event_id::None => None,
            mpv_event_id::Shutdown => Some(Ok(Event::Shutdown)),
            mpv_event_id::LogMessage => {
                let log_message = *(event.data as *mut raw::mpv_event_log_message);
                Some(
                    mpv_cstr_to_str!(CStr::from_ptr(log_message.prefix)).and_then(|prefix| {
                        Ok(Event::LogMessage {
                            prefix,
                            level: mpv_cstr_to_str!(CStr::from_ptr(log_message.level))?,
                            text: mpv_cstr_to_str!(CStr::from_ptr(log_message.text))?,
                            log_level: log_message.log_level,
                        })
                    }),
                )
            }
            mpv_event_id::GetPropertyReply => {
                let property = *(event.data as *mut raw::mpv_event_property);
                Some(
                    mpv_cstr_to_str!(CStr::from_ptr(property.name)).and_then(|name| {
                        Ok(Event::GetPropertyReply {
                            name,
                            result: PropertyData::from_raw(property.format, property.data)?,
                            reply_userdata: event.reply_userdata,
                        })
                    }),
                )
            }
            mpv_event_id::SetPropertyReply => Some(mpv_err(
                Event::SetPropertyReply(event.reply_userdata),
                event.error,
            )),
            mpv_event_id::CommandReply => Some(mpv_err(
                Event::CommandReply(event.reply_userdata),
                event.error,
            )),
            mpv_event_id::StartFile => Some(Ok(Event::StartFile)),
            mpv_event_id::EndFile => {
                let end_file = *(event.data as *mut raw::mpv_event_end_file);
                Some(if let Err(e) = mpv_err((), end_file.error) {
                    Err(e)
                } else {
                    assert!(end_file.reason.is_positive());
                    Ok(Event::EndFile(end_file.reason as _))
                })
            }
            mpv_event_id::FileLoaded => Some(Ok(Event::FileLoaded)),
            mpv_event_id::Idle => Some(Ok(Event::Idle)),
            mpv_event_id::Tick => Some(Ok(Event::Tick)),
            mpv_event_id::ClientMessage => {
                let client_message = *(event.data as *mut raw::mpv_event_client_message);
                Some(Ok(Event::ClientMessage(MessageIter(
                    slice::from_raw_parts_mut(client_message.args, client_message.num_args as _)
                        .iter()
                        .map(|msg| mpv_cstr_to_str!(CStr::from_ptr(*msg))),
                    client_message.num_args as _,
                ))))
            }
            mpv_event_id::VideoReconfig => Some(Ok(Event::VideoReconfig)),
            mpv_event_id::AudioReconfig => Some(Ok(Event::AudioReconfig)),
            mpv_event_id::Seek => Some(Ok(Event::Seek)),
            mpv_event_id::PlaybackRestart => Some(Ok(Event::PlaybackRestart)),
            mpv_event_id::PropertyChange => {
                let property = *(event.data as *mut raw::mpv_event_property);
                Some(
                    mpv_cstr_to_str!(CStr::from_ptr(property.name)).and_then(|name| {
                        Ok(Event::PropertyChange {
                            name,
                            change: PropertyData::from_raw(property.format, property.data)?,
                            reply_userdata: event.reply_userdata,
                        })
                    }),
                )
            }
            mpv_event_id::QueueOverflow => Some(Ok(Event::QueueOverflow)),
            id => Some(Ok(Event::Deprecated(id))),
        }
    }

    /// Observe `name` property for changes. `id` can be used to unobserve this (or many) properties
    /// again.
    pub fn observe_property(&self, name: &str, format: Format, id: u64) -> Result<()> {
        let name = CString::new(name)?;
        mpv_err((), unsafe {
            raw::mpv_observe_property(self.ctx.as_ptr(), id, name.as_ptr(), format.as_mpv_format() as _)
        })
    }

    /// Unobserve any property associated with `id`.
    pub fn unobserve_property(&self, id: u64) -> Result<()> {
        mpv_err((), unsafe { raw::mpv_unobserve_property(self.ctx.as_ptr(), id) })
    }
}
