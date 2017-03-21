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

// For an example of opengl callback, see pmpv.
mod events;
mod protocol;
mod _sdl2;

extern crate crossbeam;
extern crate mpv;
extern crate sdl2;

fn main() {
    if cfg!(any(feature = "all", feature = "events")) {
        events::exec();
    }
    if cfg!(any(feature = "all", feature = "protocol")) {
        protocol::exec();
    }
    if cfg!(any(feature = "all", feature = "_sdl2")) {
        _sdl2::exec();
    }
}
