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

mod errors {
    #![allow(missing_docs)]
    #[cfg(feature="events_complex")]
    use super::events::Event;
    use raw::mpv_error;
    use std::ffi::NulError;
    use std::os::raw as ctype;
    use std::str::Utf8Error;

    // FIXME: Once error_chain issue 134 is solved, this should derive Clone, and use RCs
    // instead of `Box`es. Remove temp impl Clone below then.
    error_chain!{
        foreign_links {
            Nul(NulError);
            Utf8(Utf8Error);
            Native(mpv_error);
        }

        errors {
            Loadfiles(index: usize, error: Box<Error>) {
                description("Command failed during a `loadfiles` call.")
            }
            #[cfg(feature="events_complex")]
            AlreadyObserved(e: Box<Event>) {
                description("This event is already being observed by another `EventIter`.")
            }
            InvalidArgument {
                description("An invalid argument was passed to an mpv API")
            }
            VersionMismatch(linked: ctype::c_ulong, loaded: ctype::c_ulong) {
                description("The library was compiled against a different mpv version than what is present on the system.")
            }
            InvalidUtf8 {
                description("An unspecified error during utf-8 validity checking ocurred.")
            }
            Null {
                description("Mpv returned null while creating the core.")
            }
        }
    }

    impl Clone for Error {
        #[allow(match_ref_pats)]
        fn clone(&self) -> Error {
            match self.kind() {
                &ErrorKind::Msg(ref e) => ErrorKind::Msg(e.clone()).into(),
                &ErrorKind::Nul(ref e) => ErrorKind::Nul(e.clone()).into(),
                &ErrorKind::Utf8(ref e) => ErrorKind::Utf8(*e).into(),
                &ErrorKind::Native(ref e) => ErrorKind::Native(*e).into(),
                &ErrorKind::Loadfiles(ref idx, ref err) => {
                    ErrorKind::Loadfiles(*idx, err.clone()).into()
                }
                #[cfg(feature="events_complex")]
                &ErrorKind::AlreadyObserved(ref e) => ErrorKind::AlreadyObserved(e.clone()).into(),
                &ErrorKind::InvalidArgument => ErrorKind::InvalidArgument.into(),
                &ErrorKind::VersionMismatch(ref li, ref lo) => {
                    ErrorKind::VersionMismatch(*li, *lo).into()
                }
                &ErrorKind::InvalidUtf8 => ErrorKind::InvalidUtf8.into(),
                &ErrorKind::Null => ErrorKind::Null.into(),
            }
        }
    }
}

pub use self::errors::*;

#[cfg(unix)]
macro_rules! mpv_cstr_to_str {
    ($cstr: expr) => (
        if let Some(v) = OsStr::from_bytes($cstr.to_bytes()).to_str() {
            // Not sure why the type isn't inferred
            let r: Result<&str> = Ok(v);
            r
        } else {
            Err(ErrorKind::InvalidUtf8.into())
        }
    )
}

#[cfg(all(not(unix)))]
macro_rules! mpv_cstr_to_str {
    ($cstr: expr) => (
        str::from_utf8($cstr.to_bytes())
    )
}

/// Contains event related abstractions
pub mod events;
#[cfg(feature="custom_protocols")]
/// Contains abstractions to define custom protocol handlers.
pub mod protocol;
#[cfg(feature="opengl_callback")]
/// Contains abstractions to use the opengl callback interface.
pub mod opengl_cb;

use parking_lot;
use parking_lot::Mutex;
use raw::*;

use events::*;

use std::ffi::{CStr, CString};
use std::mem;
use std::ops::Deref;
use std::os::raw as ctype;
use std::ptr;
#[cfg(any(feature="custom_protocols", feature="opengl_callback"))]
use std::sync::atomic::AtomicBool;
use std::time::Duration;
#[cfg(unix)]
use std::ffi::OsStr;
#[cfg(unix)]
use std::os::unix::ffi::OsStrExt;

fn mpv_err<T>(ret: T, err_val: ctype::c_int) -> Result<T> {
    if err_val == 0 {
        Ok(ret)
    } else {
        Err(ErrorKind::Native(mpv_error::from(err_val)).into())
    }
}

#[allow(missing_docs)]
/// This trait describes which types are allowed to be passed to getter mpv APIs.
pub unsafe trait GetData: Sized {
    #[doc(hidden)]
    fn get_from_c_void<T, F: FnMut(*mut ctype::c_void) -> Result<T>>(mut fun: F) -> Result<Self> {
        let mut ptr = unsafe { mem::zeroed() };
        let _ = fun(&mut ptr as *mut Self as _)?;
        Ok(ptr)
    }
    fn get_format() -> Format;
}

#[allow(missing_docs)]
/// This trait describes which types are allowed to be passed to setter mpv APIs.
pub unsafe trait SetData: Sized {
    #[doc(hidden)]
    fn call_as_c_void<T, F: FnMut(*mut ctype::c_void) -> Result<T>>(mut self,
                                                                    mut fun: F)
                                                                    -> Result<T> {
        fun(&mut self as *mut Self as _)
    }
    fn get_format() -> Format;
}

unsafe impl GetData for f64 {
    #[inline]
    fn get_format() -> Format {
        Format::Double
    }
}

unsafe impl SetData for f64 {
    #[inline]
    fn get_format() -> Format {
        Format::Double
    }
}

unsafe impl GetData for i64 {
    #[inline]
    fn get_format() -> Format {
        Format::Int64
    }
}

unsafe impl SetData for i64 {
    #[inline]
    fn get_format() -> Format {
        Format::Int64
    }
}

unsafe impl GetData for bool {
    #[inline]
    fn get_from_c_void<T, F: FnMut(*mut ctype::c_void) -> Result<T>>(mut fun: F) -> Result<bool> {
        let ptr = unsafe { &mut mem::zeroed() } as *mut i64;
        let _ = fun(ptr as _)?;
        Ok(match unsafe { *ptr } {
               0 => false,
               1 => true,
               _ => unreachable!(),
           })
    }

    #[inline]
    fn get_format() -> Format {
        Format::Flag
    }
}

unsafe impl SetData for bool {
    #[inline]
    fn call_as_c_void<T, F: FnMut(*mut ctype::c_void) -> Result<T>>(self, mut fun: F) -> Result<T> {
        let mut cpy: i64 = if self { 1 } else { 0 };
        fun(&mut cpy as *mut i64 as *mut _)
    }

    #[inline]
    fn get_format() -> Format {
        Format::Flag
    }
}

unsafe impl GetData for String {
    #[inline]
    fn get_from_c_void<T, F: FnMut(*mut ctype::c_void) -> Result<T>>(mut fun: F) -> Result<String> {
        let ptr = &mut ptr::null();
        let _ = fun(ptr as *mut *const ctype::c_char as _)?;

        let ret = mpv_cstr_to_str!(unsafe { CStr::from_ptr(*ptr) })?
            .to_owned();
        unsafe { mpv_free(*ptr as *mut _) };
        Ok(ret)
    }

    #[inline]
    fn get_format() -> Format {
        Format::String
    }
}

unsafe impl SetData for String {
    #[inline]
    fn call_as_c_void<T, F: FnMut(*mut ctype::c_void) -> Result<T>>(self, mut fun: F) -> Result<T> {
        let string = CString::new(self)?;
        fun((&mut string.as_ptr()) as *mut *const ctype::c_char as *mut _)
    }

    #[inline]
    fn get_format() -> Format {
        Format::String
    }
}

/// Wrapper around an `&str` returned by mpv, that properly deallocates it with mpv's allocator.
pub struct MpvStr<'a>(&'a str);
impl<'a> Deref for MpvStr<'a> {
    type Target = str;

    fn deref(&self) -> &str {
        self.0
    }
}
impl<'a> Drop for MpvStr<'a> {
    fn drop(&mut self) {
        unsafe { mpv_free(self.0.as_ptr() as *mut u8 as _) };
    }
}

unsafe impl<'a> GetData for MpvStr<'a> {
    #[inline]
    fn get_from_c_void<T, F: FnMut(*mut ctype::c_void) -> Result<T>>(mut fun: F)
                                                                     -> Result<MpvStr<'a>> {
        let ptr = &mut ptr::null();
        let _ = fun(ptr as *mut *const ctype::c_char as _)?;

        Ok(MpvStr(mpv_cstr_to_str!(unsafe { CStr::from_ptr(*ptr) })?))
    }

    #[inline]
    fn get_format() -> Format {
        Format::String
    }
}

unsafe impl<'a> SetData for &'a str {
    #[inline]
    fn call_as_c_void<T, F: FnMut(*mut ctype::c_void) -> Result<T>>(self, mut fun: F) -> Result<T> {
        let string = CString::new(self)?;
        fun((&mut string.as_ptr()) as *mut *const ctype::c_char as *mut _)
    }

    #[inline]
    fn get_format() -> Format {
        Format::String
    }
}

#[derive(Clone)]
/// An `OsdString` can only be used by getter functions, and is subject to [property expansion](https://mpv.io/manual/master/#property-expansion).
pub struct OsdString(String);
impl Deref for OsdString {
    type Target = String;

    fn deref(&self) -> &String {
        &self.0
    }
}

unsafe impl<'a> GetData for OsdString {
    #[inline]
    fn get_from_c_void<T, F: FnMut(*mut ctype::c_void) -> Result<T>>(fun: F) -> Result<OsdString> {
        Ok(OsdString(String::get_from_c_void(fun)?))
    }

    #[inline]
    fn get_format() -> Format {
        Format::OsdString
    }
}

unsafe impl<'a> SetData for OsdString {
    #[inline]
    fn call_as_c_void<T, F: FnMut(*mut ctype::c_void) -> Result<T>>(self, fun: F) -> Result<T> {
        self.0.call_as_c_void(fun)
    }

    #[inline]
    fn get_format() -> Format {
        Format::OsdString
    }
}

#[allow(missing_docs)]
#[derive(Debug, Clone)]
/// Subset of `mpv_format` used by the public API.
pub enum Format {
    String,
    OsdString,
    Flag,
    Int64,
    Double,
}

impl Format {
    fn as_mpv_format(&self) -> mpv_format {
        match *self {
            Format::String => mpv_format::MPV_FORMAT_STRING,
            Format::OsdString => mpv_format::MPV_FORMAT_OSD_STRING,
            Format::Flag => mpv_format::MPV_FORMAT_FLAG,
            Format::Int64 => mpv_format::MPV_FORMAT_INT64,
            Format::Double => mpv_format::MPV_FORMAT_DOUBLE,
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

/// TODO
pub struct Mpv {
    /// The handle to the mpv core
    pub ctx: *mut mpv_handle,
    #[cfg(feature="events_complex")]
    ev_iter_notification: Box<(Mutex<bool>, parking_lot::Condvar)>,
    #[cfg(feature="events_complex")]
    ev_to_observe: Mutex<Vec<Event>>,
    #[cfg(feature="events_complex")]
    ev_to_observe_properties: Mutex<::std::collections::HashMap<String, u64>>,
    #[cfg(feature="events_complex")]
    ev_observed: Mutex<Vec<Event>>,
    #[cfg(feature="custom_protocols")]
    protocols_guard: AtomicBool,
    #[cfg(feature="opengl_callback")]
    opengl_guard: AtomicBool,
}

unsafe impl Send for Mpv {}
unsafe impl Sync for Mpv {}

impl Drop for Mpv {
    #[inline]
    fn drop(&mut self) {
        unsafe {
            mpv_terminate_destroy(self.ctx);
        }
    }
}

impl Mpv {
    #[cfg(not(feature="events_complex"))]
    #[inline]
    /// Create a new `Mpv`.
    /// The default settings can be probed by running: `$ mpv --show-profile=libmpv`
    pub fn new() -> Result<Mpv> {
        let api_version = unsafe { mpv_client_api_version() };
        if ::MPV_CLIENT_API_VERSION != api_version {
            return Err(ErrorKind::VersionMismatch(::MPV_CLIENT_API_VERSION, api_version).into());
        }

        let ctx = unsafe { mpv_create() };
        if ctx.is_null() {
            return Err(ErrorKind::Null.into());
        }
        mpv_err((), unsafe { mpv_initialize(ctx) })
            .or_else(|err| {
                         unsafe { mpv_terminate_destroy(ctx) };
                         Err(err)
                     })?;

        // TODO: This can be made much prettier once `struct_field_attributes` is stable.
        let ret;
#[cfg(all(feature="custom_protocols", not(feature="opengl_callback")))]        {
            ret = Ok(Mpv {
                         ctx,
                         protocols_guard: AtomicBool::new(false),
                     });
        }
#[cfg(all(feature="opengl_callback", not(feature="custom_protocols")))]        {
            ret = Ok(Mpv {
                         ctx,
                         opengl_guard: AtomicBool::new(false),
                     });
        }
#[cfg(all(feature="opengl_callback", feature="custom_protocols"))]        {
            ret = Ok(Mpv {
                         ctx,
                         protocols_guard: AtomicBool::new(false),
                         opengl_guard: AtomicBool::new(false),
                     });
        }
#[cfg(all(not(feature="opengl_callback"), not(feature="custom_protocols")))]        {
            ret = Ok(Mpv { ctx });
        }
        ret
    }

    #[inline]
    /// Load a configuration file. The path has to be absolute, and a file.
    pub fn load_config(&self, path: &str) -> Result<()> {
        let file = CString::new(path)?.into_raw();
        let ret = mpv_err((), unsafe { mpv_load_config_file(self.ctx, file) });
        unsafe { CString::from_raw(file) };
        ret
    }

    #[inline]
    /// Send a command to the `Mpv` instance. This uses `mpv_command_string` internally,
    /// so that the syntax is the same as described in the [manual for the input.conf]
    /// (https://mpv.io/manual/master/#list-of-input-commands).
    ///
    /// Note that you may have to escape strings with `""` when they contain spaces.
    ///
    /// # Safety
    /// This method is unsafe because arbitrary code may be executed resulting in UB and more.
    pub unsafe fn command(&self, name: &str, args: &[&str]) -> Result<()> {
        let mut cmd = String::with_capacity(name.len() +
                                            args.iter().fold(0, |acc, e| acc + e.len() + 1));
        cmd.push_str(name);

        for elem in args {
            cmd.push_str(" ");
            cmd.push_str(elem);
        }
        let raw = CString::new(cmd)?;

        mpv_err((), mpv_command_string(self.ctx, raw.as_ptr()))
    }

    #[inline]
    /// Set the value of a property.
    pub fn set_property<T: SetData>(&self, name: &str, data: T) -> Result<()> {
        let name = CString::new(name)?;
        let format = T::get_format().as_mpv_format() as _;
        data.call_as_c_void(|ptr| {
                                mpv_err((), unsafe {
                mpv_set_property(self.ctx, name.as_ptr(), format, ptr)
            })
                            })
    }

    #[inline]
    /// Get the value of a property.
    pub fn get_property<T: GetData>(&self, name: &str) -> Result<T> {
        let name = CString::new(name)?;

        let format = T::get_format().as_mpv_format() as _;
        T::get_from_c_void(|ptr| {
                               mpv_err((), unsafe {
                mpv_get_property(self.ctx, name.as_ptr(), format, ptr)
            })
                           })
    }

    #[inline]
    /// Internal time in microseconds, this has an arbitrary offset, and will never go backwards.
    ///
    /// This can be called at any time, even if it was stated that no API function should be called.
    pub fn get_internal_time(&self) -> i64 {
        unsafe { mpv_get_time_us(self.ctx) }
    }

    // --- Convenience property functions ---
    //

    #[inline]
    /// Add -or subtract- any value from a property. Over/underflow clamps to max/min.
    pub fn add_property(&self, property: &str, value: isize) -> Result<()> {
        unsafe { self.command("add", &[property, &format!("{}", value)]) }
    }

    #[inline]
    /// Cycle through a given property. `up` specifies direction. On
    /// overflow, set the property back to the minimum, on underflow set it to the maximum.
    pub fn cycle_property(&self, property: &str, up: bool) -> Result<()> {
        unsafe { self.command("cycle", &[property, if up { "up" } else { "down" }]) }
    }

    #[inline]
    /// Multiply any property with any positive factor.
    pub fn multiply_property(&self, property: &str, factor: usize) -> Result<()> {
        unsafe { self.command("multiply", &[property, &format!("{}", factor)]) }
    }

    #[inline]
    /// Pause playback at runtime.
    pub fn pause(&self) -> Result<()> {
        self.set_property("pause", true)
    }

    #[inline]
    /// Unpause playback at runtime.
    pub fn unpause(&self) -> Result<()> {
        self.set_property("pause", false)
    }

    // --- Convenience command functions ---
    //

    // --- Seek functions ---
    //

    #[inline]
    /// Seek forward relatively from current position at runtime.
    /// This is less exact than `seek_absolute`, see [mpv manual]
    /// (https://mpv.io/manual/master/#command-interface-
    /// [relative|absolute|absolute-percent|relative-percent|exact|keyframes]).
    pub fn seek_forward(&self, time: &Duration) -> Result<()> {
        unsafe { self.command("seek", &[&format!("{}", time.as_secs()), "relative"]) }
    }

    #[inline]
    /// See `seek_forward`.
    pub fn seek_backward(&self, time: &Duration) -> Result<()> {
        unsafe { self.command("seek", &[&format!("-{}", time.as_secs()), "relative"]) }
    }

    #[inline]
    /// Seek to a given absolute time.
    pub fn seek_absolute(&self, time: &Duration) -> Result<()> {
        unsafe { self.command("seek", &[&format!("{}", time.as_secs()), "absolute"]) }
    }

    #[inline]
    /// Seek to a given relative percent position (may be negative).
    /// If `percent` of the playtime is bigger than the remaining playtime, the next file is played.
    /// out of bounds values are clamped to either 0 or 100.
    pub fn seek_percent(&self, percent: isize) -> Result<()> {
        unsafe { self.command("seek", &[&format!("{}", percent), "relative-percent"]) }
    }

    #[inline]
    /// Seek to the given percentage of the playtime.
    pub fn seek_percent_absolute(&self, percent: usize) -> Result<()> {
        unsafe { self.command("seek", &[&format!("{}", percent), "relative-percent"]) }
    }

    #[inline]
    /// Revert the previous `seek_` call, can also revert itself.
    pub fn seek_revert(&self) -> Result<()> {
        unsafe { self.command("revert-seek", &[]) }
    }

    #[inline]
    /// Mark the current position as the position that will be seeked to by `seek_revert`.
    pub fn seek_revert_mark(&self) -> Result<()> {
        unsafe { self.command("revert-seek", &["mark"]) }
    }

    #[inline]
    /// Seek exactly one frame, and pause.
    /// Noop on audio only streams.
    pub fn seek_frame(&self) -> Result<()> {
        unsafe { self.command("frame-step", &[]) }
    }

    #[inline]
    /// See `seek_frame`.
    /// [Note performance considerations.](https://mpv.io/manual/master/#command-interface-frame-back-step)
    pub fn seek_frame_backward(&self) -> Result<()> {
        unsafe { self.command("frame-back-step", &[]) }
    }

    // --- Screenshot functions ---
    //

    #[inline]
    /// "Save the video image, in its original resolution, and with subtitles.
    /// Some video outputs may still include the OSD in the output under certain circumstances.".
    ///
    /// "[O]ptionally save it to a given file. The format of the file will be
    /// guessed by the extension (and --screenshot-format is ignored - the behaviour when the
    /// extension is missing or unknown is arbitrary). If the file already exists, it's overwritten.
    /// Like all input command parameters, the filename is subject to property expansion as
    /// described in [Property Expansion](https://mpv.io/manual/master/#property-expansion)."
    pub fn screenshot_subtitles<'a, A: Into<Option<&'a str>>>(&self, path: A) -> Result<()> {
        if let Some(path) = path.into() {
            unsafe { self.command("screenshot", &[&format!("\"{}\"", path), "subtitles"]) }
        } else {
            unsafe { self.command("screenshot", &["subtitles"]) }
        }
    }

    #[inline]
    /// "Like subtitles, but typically without OSD or subtitles. The exact behavior depends on the selected
    /// video output."
    pub fn screenshot_video<'a, A: Into<Option<&'a str>>>(&self, path: A) -> Result<()> {
        if let Some(path) = path.into() {
            unsafe { self.command("screenshot", &[&format!("\"{}\"", path), "video"]) }
        } else {
            unsafe { self.command("screenshot", &["video"]) }
        }
    }

    #[inline]
    /// "Save the contents of the mpv window. Typically scaled, with OSD and subtitles. The exact
    /// behaviour depends on the selected video output, and if no support is available,
    /// this will act like video.".
    pub fn screenshot_window<'a, A: Into<Option<&'a str>>>(&self, path: A) -> Result<()> {
        if let Some(path) = path.into() {
            unsafe { self.command("screenshot", &[&format!("\"{}\"", path), "window"]) }
        } else {
            unsafe { self.command("screenshot", &["window"]) }
        }
    }

    // --- Playlist functions ---
    //

    #[inline]
    /// Play the next item of the current playlist.
    /// Does nothing if the current item is the last item.
    pub fn playlist_next_weak(&self) -> Result<()> {
        unsafe { self.command("playlist-next", &["weak"]) }
    }

    #[inline]
    /// Play the next item of the current playlist.
    /// Terminates playback if the current item is the last item.
    pub fn playlist_next_force(&self) -> Result<()> {
        unsafe { self.command("playlist-next", &["force"]) }
    }

    #[inline]
    /// See `playlist_next_weak`.
    pub fn playlist_previous_weak(&self) -> Result<()> {
        unsafe { self.command("playlist-previous", &["weak"]) }
    }

    #[inline]
    /// See `playlist_next_force`.
    pub fn playlist_previous_force(&self) -> Result<()> {
        unsafe { self.command("playlist-previous", &["force"]) }
    }

    #[inline]
    /// The given files are loaded sequentially, returning the index of the current file
    /// and the error in case of an error. [More information.](https://mpv.io/manual/master/#command-interface-[replace|append|append-play)
    ///
    /// # Arguments
    /// The tuple consists of:
    ///     * a string slice - the path
    ///     * a `FileState` - how the file will be opened
    ///     * an optional string slice - any additional options that will be set for this file
    ///
    /// # Peculiarities
    /// `loadfile` is kind of asynchronous, any additional option is set during loading, [specifics](https://github.com/mpv-player/mpv/issues/4089).
    pub fn playlist_load_files<'a, A>(&self, files: &[(&str, FileState, A)]) -> Result<()>
        where A: Into<Option<&'a str>> + Clone
    {
        for (i, elem) in files.iter().enumerate() {
            let args = elem.2.clone().into().unwrap_or("");

            let ret = unsafe {
                self.command("loadfile",
                             &[&format!("\"{}\"", elem.0), elem.1.val(), args])
            };

            if ret.is_err() {
                return Err(ErrorKind::Loadfiles(i, Box::new(ret.unwrap_err())).into());
            }
        }
        Ok(())
    }

    #[inline]
    /// Load the given playlist file, that either replaces the current playlist, or appends to it.
    pub fn playlist_load_list(&self, path: &str, replace: bool) -> Result<()> {
        if replace {
            unsafe { self.command("loadlist", &[&format!("\"{}\"", path), "replace"]) }
        } else {
            unsafe { self.command("loadlist", &[&format!("\"{}\"", path), "append"]) }
        }
    }

    #[inline]
    /// Remove every, except the current, item from the playlist.
    pub fn playlist_clear(&self) -> Result<()> {
        unsafe { self.command("playlist-clear", &[]) }
    }

    #[inline]
    /// Remove the currently selected item from the playlist.
    pub fn playlist_remove_current(&self) -> Result<()> {
        unsafe { self.command("playlist-remove", &["current"]) }
    }

    #[inline]
    /// Remove item at `position` from the playlist.
    pub fn playlist_remove_index(&self, position: usize) -> Result<()> {
        unsafe { self.command("playlist-remove", &[&format!("{}", position)]) }
    }

    #[inline]
    /// Move item `old` to the position of item `new`.
    pub fn playlist_move(&self, old: usize, new: usize) -> Result<()> {
        unsafe { self.command("playlist-move", &[&format!("{}", new), &format!("{}", old)]) }
    }

    #[inline]
    /// Shuffle the playlist.
    pub fn playlist_shuffle(&self) -> Result<()> {
        unsafe { self.command("playlist-shuffle", &[]) }
    }

    // --- Subtitle functions ---
    //

    #[inline]
    /// Add and select the subtitle immediately.
    /// Specifying a language requires specifying a title.
    pub fn subtitle_add_select<'a, 'b, A: Into<Option<&'a str>>, B: Into<Option<&'b str>>>
        (&self,
         path: &str,
         title: A,
         lang: B)
         -> Result<()> {
        match (title.into(), lang.into()) {
            (None, None) => unsafe {
                self.command("sub-add", &[&format!("\"{}\"", path), "select"])
            },
            (Some(t), None) => unsafe {
                self.command("sub-add", &[&format!("\"{}\"", path), "select", t])
            },
            (None, Some(_)) => Err(ErrorKind::InvalidArgument.into()),
            (Some(t), Some(l)) => unsafe {
                self.command("sub-add", &[&format!("\"{}\"", path), "select", t, l])
            },
        }
    }

    #[inline]
    /// See `AddSelect`. "Don't select the subtitle.
    /// (Or in some special situations, let the default stream selection mechanism decide.)".
    ///
    /// Returns an `Error::InvalidArgument` if a language, but not a title, was provided.
    pub fn subtitle_add_auto<'a, 'b, A: Into<Option<&'a str>>, B: Into<Option<&'b str>>>
        (&self,
         path: &str,
         title: A,
         lang: B)
         -> Result<()> {
        match (title.into(), lang.into()) {
            (None, None) => unsafe { self.command("sub-add", &[&format!("\"{}\"", path), "auto"]) },
            (Some(t), None) => unsafe {
                self.command("sub-add", &[&format!("\"{}\"", path), "auto", t])
            },
            (Some(t), Some(l)) => unsafe {
                self.command("sub-add", &[&format!("\"{}\"", path), "auto", t, l])
            },
            (None, Some(_)) => Err(ErrorKind::InvalidArgument.into()),
        }
    }

    #[inline]
    /// See `AddSelect`. "Select the subtitle. If a subtitle with the same file name was
    /// already added, that one is selected, instead of loading a duplicate entry.
    /// (In this case, title/language are ignored, and if the [sub] was changed since it was loaded,
    /// these changes won't be reflected.)".
    pub fn subtitle_add_cached(&self, path: &str) -> Result<()> {
        unsafe { self.command("sub-add", &[&format!("\"{}\"", path), "cached"]) }
    }

    #[inline]
    /// "Remove the given subtitle track. If the id argument is missing, remove the current
    /// track. (Works on external subtitle files only.)"
    pub fn subtitle_remove<A: Into<Option<usize>>>(&self, index: A) -> Result<()> {
        if let Some(idx) = index.into() {
            unsafe { self.command("sub-remove", &[&format!("{}", idx)]) }
        } else {
            unsafe { self.command("sub-remove", &[]) }
        }
    }

    #[inline]
    /// "Reload the given subtitle track. If the id argument is missing, reload the current
    /// track. (Works on external subtitle files only.)"
    pub fn subtitle_reload<A: Into<Option<usize>>>(&self, index: A) -> Result<()> {
        if let Some(idx) = index.into() {
            unsafe { self.command("sub-reload", &[&format!("{}", idx)]) }
        } else {
            unsafe { self.command("sub-reload", &[]) }
        }
    }

    #[inline]
    /// "Change subtitle timing such, that the subtitle event after the next `isize` subtitle
    /// events is displayed. `isize` can be negative to step backwards."
    pub fn subtitle_step(&self, skip: isize) -> Result<()> {
        unsafe { self.command("sub-step", &[&format!("{}", skip)]) }
    }

    #[inline]
    /// "Seek to the next subtitle. This is similar to sub-step, except that it seeks video and
    /// audio instead of adjusting the subtitle delay.
    /// For embedded subtitles (like with matroska), this works only with subtitle events that
    /// have already been displayed, or are within a short prefetch range."
    pub fn subtitle_seek_forward(&self) -> Result<()> {
        unsafe { self.command("sub-seek", &["1"]) }
    }

    #[inline]
    /// See `SeekForward`.
    pub fn subtitle_seek_backward(&self) -> Result<()> {
        unsafe { self.command("sub-seek", &["-1"]) }
    }
}
