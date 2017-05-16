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

use std::collections::HashMap;
use std::marker::PhantomData;

unsafe extern "C" fn event_callback(d: *mut libc::c_void) {
    (*(d as *mut Condvar)).notify_one();
}

impl Mpv {
    #[cfg(feature="events_complex")]
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

        let (ev_iter_notification, ev_to_observe, ev_to_observe_properties, ev_observed) = {
            let ev_iter_notification = Box::new((Mutex::new(false), Condvar::new()));
            unsafe {
                mpv_set_wakeup_callback(ctx,
                                        event_callback,
                                        &ev_iter_notification.1 as *const Condvar as *mut Condvar as
                                        *mut _);
            }

            (ev_iter_notification,
             Mutex::new(Vec::with_capacity(10)),
             Mutex::new(HashMap::with_capacity(10)),
             Mutex::new(Vec::with_capacity(15)))
        };

        for i in 2..24 {
            if let Err(e) = mpv_err((), unsafe {
                mpv_request_event(ctx, MpvEventId::from_i32(i).unwrap(), 0)
            }) {
                unsafe { mpv_terminate_destroy(ctx) };
                return Err(e);
            }
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
                         ev_iter_notification,
                         ev_to_observe,
                         ev_to_observe_properties,
                         ev_observed,
                         protocols_guard: AtomicBool::new(false),
                     });
        }
#[cfg(all(feature="opengl_callback", not(feature="custom_protocols")))]        {
            ret = Ok(Mpv {
                         ctx,
                         ev_iter_notification,
                         ev_to_observe,
                         ev_to_observe_properties,
                         ev_observed,
                         opengl_guard: AtomicBool::new(false),
                     });
        }
#[cfg(all(feature="opengl_callback", feature="custom_protocols"))]        {
            ret = Ok(Mpv {
                         ctx,
                         ev_iter_notification,
                         ev_to_observe,
                         ev_to_observe_properties,
                         ev_observed,
                         protocols_guard: AtomicBool::new(false),
                         opengl_guard: AtomicBool::new(false),
                     });
        }
#[cfg(all(not(feature="opengl_callback"), not(feature="custom_protocols")))]        {
            ret = Ok(Mpv {
                         ctx,
                         ev_iter_notification,
                         ev_to_observe,
                         ev_to_observe_properties,
                         ev_observed,
                     });
        }
        ret
    }

    #[inline]
    /// Observe given `Event`s via an `EventIter`.
    pub fn observe_events(&self, events: &[Event]) -> Result<EventIter> {
        let mut observe = self.ev_to_observe.lock();
        let mut properties = self.ev_to_observe_properties.lock();

        let len = events.len();
        // FIXME: This can be alloca'ed once the RFC is implemented
        let mut ids = Vec::with_capacity(len);
        let mut evs = Vec::with_capacity(len);
        let mut props = Vec::with_capacity(len);
        for elem in events {
            if let Event::PropertyChange(ref v) = *elem {
                if properties.contains_key(&v.0) {
                    return Err(ErrorKind::AlreadyObserved(Box::new(elem.clone())).into());
                } else {
                    mpv_err((), unsafe { mpv_request_event(self.ctx, elem.as_id(), 1) })?;
                    props.push(v);
                    ids.push(elem.as_id());
                    evs.push(elem.clone());
                }
            } else {
                for id in &*observe {
                    if elem.as_id() == id.as_id() {
                        return Err(ErrorKind::AlreadyObserved(Box::new(elem.clone())).into());
                    }
                }

                if let Event::LogMessage { level: lvl, .. } = *elem {
                    let min_level = CString::new(lvl.as_str())?;
                    mpv_err((),
                            unsafe { mpv_request_log_messages(self.ctx, min_level.as_ptr()) })?;
                }

                mpv_err((), unsafe { mpv_request_event(self.ctx, elem.as_id(), 1) })?;
                ids.push(elem.as_id());
                evs.push(elem.clone());
            }
        }

        let mut props_ins = Vec::with_capacity(len);
        let start_id = properties.len();
        for (i, elem) in props.iter().enumerate() {
            let name = CString::new(&elem.0[..])?;
            let err = mpv_err((), unsafe {
                mpv_observe_property(self.ctx,
                                     (start_id + i) as _,
                                     name.as_ptr(),
                                     elem.1.format() as _)
            });
            if err.is_err() {
                for (_, id) in props_ins {
                    // Ignore errors.
                    unsafe { mpv_unobserve_property(self.ctx, id) };
                }
                return Err(err.unwrap_err());
            }
            props_ins.push((elem.0.clone(), (start_id + i) as _));
        }
        observe.extend(evs.clone());
        properties.extend(props_ins);

        Ok(EventIter {
               ctx: self.ctx,
               first_iteration: true,
               notification: &self.ev_iter_notification,
               all_to_observe: &self.ev_to_observe,
               all_to_observe_properties: &self.ev_to_observe_properties,
               local_to_observe: evs,
               all_observed: &self.ev_observed,
               _does_not_outlive: PhantomData::<&Self>,
           })
    }
}

/// A blocking `Iterator` over some observed events of an `Mpv` instance.
/// Once the `EventIter` is dropped, it's `Event`s are removed from
/// the "to be observed" queue, therefore new `Event` invocations won't be observed.
pub struct EventIter<'parent> {
    ctx: *mut MpvHandle,
    first_iteration: bool,
    notification: &'parent (Mutex<bool>, Condvar),
    all_to_observe: &'parent Mutex<Vec<Event>>,
    all_to_observe_properties: &'parent Mutex<HashMap<String, libc::uint64_t>>,
    local_to_observe: Vec<Event>,
    all_observed: &'parent Mutex<Vec<Event>>,
    _does_not_outlive: PhantomData<&'parent Mpv>,
}

impl<'parent> Drop for EventIter<'parent> {
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
                    let min_level = &*b"no\0";
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

impl<'parent> Iterator for EventIter<'parent> {
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
