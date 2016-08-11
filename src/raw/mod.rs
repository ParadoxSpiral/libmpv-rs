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

#![allow(dead_code, improper_ctypes, missing_docs)]
use libc;

pub mod prototype {
    #[repr(C)]
    /// This is ok, because you only ever need a `*mut MpvHandle`.
    pub struct MpvHandle;
}

enum_from_primitive! {
    #[repr(C)]
    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub enum MpvError {
        // Success = 0, Handled internally
        EventQueueFull = -1,
        NoMem = -2,
        Uninitialized = -3,
        InvalidParameter = -4,
        OptionNotFound = -5,
        OptionFormat = -6,
        OptionError = -7,
        PropertyNotFound = -8,
        PropertyFormat = -9,
        PropertyUnavailable = -10,
        PropertyError = -11,
        Command = -12,
        LoadingFailed = -13,
        AoInitFailed = -14,
        VoInitFailed = -15,
        NothingToPlay = -16,
        UnknownFormat = -17,
        Unsupported = -18,
        NotImplemented = -19,
        Generic = -20,
    }
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MpvFormat {
    None = 0,
    String = 1,
    OsdString = 2,
    Flag = 3,
    Int64 = 4,
    Double = 5,
    Node = 6,
    NodeArray = 7,
    NodeMap = 8,
    ByteArray = 9,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, Eq)]
pub struct MpvNode {
    // FIXME:
    // union {
    // char *string;
    // int flag;
    // int64_t int64;
    // double double_;
    // struct mpv_node_list *list;
    // struct mpv_byte_array *ba;
    // } u;
    pub format: MpvFormat,
}

#[repr(C)]
#[derive(Debug)]
pub struct MpvNodeList {
    pub num: libc::c_int,
    pub values: *mut MpvNode,
    pub keys: *mut *mut libc::c_char,
}

#[repr(C)]
#[derive(Debug)]
pub struct MpvByteArray {
    pub data: *mut libc::c_void,
    pub size: libc::size_t,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MpvEventId {
    None = 0,
    Shutdown = 1,
    LogMessage = 2,
    GetPropertyReply = 3,
    SetPropertyReply = 4,
    CommandReply = 5,
    StartFile = 6,
    EndFile = 7,
    FileLoaded = 8,
    TracksChanged = 9,
    TrackSwitched = 10,
    Idle = 11,
    Pause = 12,
    Unpause = 13,
    Tick = 14,
    ScriptInputDispatch = 15,
    ClientMessage = 16,
    VideoReconfig = 17,
    AudioReconfig = 18,
    MetadataUpdate = 19,
    Seek = 20,
    PlaybackRestart = 21,
    PropertyChange = 22,
    ChapterChange = 23,
    QueueOverflow = 24,
}

#[repr(C)]
#[derive(Debug)]
pub struct MpvEventProperty {
    pub name: *const libc::c_char,
    pub format: MpvFormat,
    pub data: *mut libc::c_void,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MpvLogLevel {
    None = 0,
    Fatal = 10,
    Error = 20,
    Warn = 30,
    Info = 40,
    V = 50,
    Debug = 60,
    Trace = 70,
}

#[repr(C)]
pub struct MpvEventLogMessage {
    pub prefix: *const libc::c_char,
    pub level: *const libc::c_char,
    pub text: *const libc::c_char,
    pub log_level: MpvLogLevel,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MpvEndFileReason {
    Eof = 0,
    Stop = 2,
    Quit = 3,
    Error = 4,
    Redirect = 5,
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MpvEventEndFile {
    pub reason: libc::c_int,
    pub error: libc::c_int,
}

#[repr(C)]
pub struct MpvEventScriptInputDispatch {
    pub arg0: libc::c_int,
    pub type_: *const libc::c_char,
}

#[repr(C)]
pub struct MpvEventClientMessage {
    pub num_args: libc::c_int,
    pub args: *mut *const libc::c_char,
}

#[repr(C)]
#[derive(Debug)]
pub struct MpvEvent {
    pub event_id: MpvEventId,
    pub error: libc::c_int,
    pub reply_userdata: libc::uint64_t,
    pub data: *mut libc::c_void,
}

// Needed because used in lazy_static.
unsafe impl Send for MpvEvent {}
unsafe impl Sync for MpvEvent {}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MpvSubApi {
    OpenglCb = 1,
}

#[cfg_attr(feature="static", link(name = "mpv", kind = "static"))]
#[cfg_attr(not(feature="static"), link(name = "mpv"))]
extern "C" {
    pub fn mpv_client_api_version() -> u32;
    pub fn mpv_error_string(error: libc::c_int) -> *const libc::c_char;
    pub fn mpv_free(data: *mut libc::c_void);
    pub fn mpv_client_name(ctx: *const prototype::MpvHandle) -> *const libc::c_char;
    pub fn mpv_create() -> *mut prototype::MpvHandle;
    pub fn mpv_initialize(ctx: *mut prototype::MpvHandle) -> libc::c_int;
    pub fn mpv_detach_destroy(ctx: *mut prototype::MpvHandle);
    pub fn mpv_terminate_destroy(ctx: *mut prototype::MpvHandle);
    pub fn mpv_create_client(ctx: *mut prototype::MpvHandle,
                             name: *const libc::c_char)
                             -> *mut prototype::MpvHandle;
    pub fn mpv_load_config_file(ctx: *mut prototype::MpvHandle,
                                filename: *const libc::c_char)
                                -> libc::c_int;
    pub fn mpv_suspend(ctx: *mut prototype::MpvHandle);
    pub fn mpv_resume(ctx: *mut prototype::MpvHandle);
    pub fn mpv_free_node_contents(node: *mut MpvNode);
    pub fn mpv_set_option(ctx: *mut prototype::MpvHandle,
                          name: *const libc::c_char,
                          format: libc::c_int,
                          data: *mut libc::c_void)
                          -> libc::c_int;
    pub fn mpv_set_option_string(ctx: *mut prototype::MpvHandle,
                                 name: *const libc::c_char,
                                 data: *const libc::c_char)
                                 -> libc::c_int;
    pub fn mpv_command(ctx: *mut prototype::MpvHandle,
                       args: *mut *const libc::c_char)
                       -> libc::c_int;
    pub fn mpv_command_node(ctx: *mut prototype::MpvHandle,
                            args: *mut MpvNode,
                            result: *mut MpvNode)
                            -> libc::c_int;
    pub fn mpv_command_string(ctx: *mut prototype::MpvHandle,
                              args: *const libc::c_char)
                              -> libc::c_int;
    pub fn mpv_get_time_us(ctx: *mut prototype::MpvHandle) -> libc::int64_t;
    pub fn mpv_command_async(ctx: *mut prototype::MpvHandle,
                             reply_userdata: libc::uint64_t,
                             args: *const *const libc::c_char)
                             -> libc::c_int;
    pub fn mpv_set_property(ctx: *mut prototype::MpvHandle,
                            name: *const libc::c_char,
                            format: libc::c_int,
                            data: *mut libc::c_void)
                            -> libc::c_int;
    pub fn mpv_set_property_string(ctx: *mut prototype::MpvHandle,
                                   name: *const libc::c_char,
                                   data: *const libc::c_char)
                                   -> libc::c_int;
    pub fn mpv_set_property_async(ctx: *mut prototype::MpvHandle,
                                  reply_userdata: libc::uint64_t,
                                  name: *const libc::c_char,
                                  format: libc::c_int,
                                  data: *mut libc::c_void)
                                  -> libc::c_int;
    pub fn mpv_get_property(ctx: *mut prototype::MpvHandle,
                            name: *const libc::c_char,
                            format: libc::c_int,
                            data: *mut libc::c_void)
                            -> libc::c_int;
    pub fn mpv_get_property_string(ctx: *mut prototype::MpvHandle,
                                   name: *const libc::c_char)
                                   -> *mut libc::c_char;
    pub fn mpv_get_property_osd_string(ctx: *mut prototype::MpvHandle,
                                       name: *const libc::c_char)
                                       -> *mut libc::c_char;
    pub fn mpv_get_property_async(ctx: *mut prototype::MpvHandle,
                                  reply_userdata: libc::uint64_t,
                                  name: *const libc::c_char,
                                  format: libc::c_int)
                                  -> libc::c_int;
    pub fn mpv_observe_property(mpv: *mut prototype::MpvHandle,
                                reply_userdata: libc::uint64_t,
                                name: *const libc::c_char,
                                format: libc::c_int)
                                -> libc::c_int;
    pub fn mpv_unobserve_property(mpv: *mut prototype::MpvHandle,
                                  registered_reply_userdata: libc::uint64_t)
                                  -> libc::c_int;
    pub fn mpv_event_name(event: MpvEventId) -> *const libc::c_char;
    pub fn mpv_request_event(ctx: *mut prototype::MpvHandle,
                             event: MpvEventId,
                             enable: libc::c_int)
                             -> libc::c_int;
    pub fn mpv_request_log_messages(ctx: *mut prototype::MpvHandle,
                                    min_level: *const libc::c_char)
                                    -> libc::c_int;
    pub fn mpv_wait_event(ctx: *mut prototype::MpvHandle,
                          timeout: libc::c_double)
                          -> *mut MpvEvent;
    pub fn mpv_wakeup(ctx: *mut prototype::MpvHandle);
    pub fn mpv_set_wakeup_callback(ctx: *mut prototype::MpvHandle,
                                   cb: unsafe extern "C" fn(*mut libc::c_void),
                                   d: *mut libc::c_void);
    pub fn mpv_get_wakeup_pipe(ctx: *mut prototype::MpvHandle) -> libc::c_int;
    pub fn mpv_wait_async_requests(ctx: *mut prototype::MpvHandle);
    pub fn mpv_get_sub_api(ctx: *mut prototype::MpvHandle, sub_api: MpvSubApi);
}
