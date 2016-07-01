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

use super::*;
use test::Bencher;

#[test]
fn version() {
    assert_eq!(super::MPV_CLIENT_API_VERSION,
               unsafe { raw::mpv_client_api_version() });
}

#[test]
fn options_properties() {
    // TODO: Cover all `Data` variants.

    let mpv = Parent::new(false).unwrap();
    mpv.set_option(Property::new("volume", Data::new(0))).unwrap();
    mpv.init().unwrap();

    assert_eq!(Data::new(0),
               mpv.get_property("volume", &Format::Int64)
                  .unwrap());

    mpv.set_property(Property::new("volume", Data::new(4))).unwrap();

    assert_eq!(Data::new(4),
               mpv.get_property("volume", &Format::Int64)
                  .unwrap());
}

#[bench]
fn mpv_error(b: &mut Bencher) {
  let n = super::test::black_box(0);

  b.iter(|| {
    for n in -19...n {
      super::wrapper::mpv_err((), n);
    }
  });
}
