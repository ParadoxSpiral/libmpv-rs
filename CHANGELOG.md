# Changelog

## Unreleased
* Add method `Mpv::with_initializer` to set options before initialization
* [breaking] Borrow `&mut self` in `wait_event` to disallow using two events where the first points to data freed in the second `wait_event` call
* [breaking] `PropertyData<'a>` no longer `Clone` or `PartialEq`, `Event<'a>` no longer `Clone` to avoid cloning/comparing `MpvNode`

## Version 1.1.0
* Add an `MpvNode` that implements `GetData`, i.a. with `MpvNodeArrayIter` and `MpvNodeMapIter` variants that support e.g. properties `audio-parmas` and `playlist`

## Version 1.0.1
* Use debug formatting in impl of `Display` trait for `Error`
