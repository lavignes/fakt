use crate::collections::XorHashMap;
use std::ptr::slice_from_raw_parts;
use std::rc::Rc;
use std::sync::Arc;
use std::{hash::Hash, marker::PhantomData, str};

#[derive(Debug, Eq, PartialEq)]
pub struct Interned<T: ?Sized> {
    slice_id: usize,
    marker: PhantomData<T>,
}

pub unsafe trait Internable {
    fn to_bytes(&self) -> &[u8];
    fn from_bytes(bytes: &[u8]) -> &Self;
}

unsafe impl Internable for str {
    fn to_bytes(&self) -> &[u8] {
        self.as_bytes()
    }

    fn from_bytes(bytes: &[u8]) -> &str {
        unsafe { str::from_utf8_unchecked(bytes) }
    }
}

pub struct Interner<T>
where
    T: Internable + Eq + Hash + ?Sized + 'static,
{
    map: XorHashMap<&'static T, usize>,
    slices: Vec<&'static [u8]>,
    buffers: Vec<Vec<u8>>,
}

impl<T> Interner<T>
where
    T: Internable + Eq + Hash + ?Sized,
{
    pub fn new() -> Interner<T> {
        Interner {
            map: XorHashMap::default(),
            slices: Vec::new(),
            buffers: vec![Vec::with_capacity(32)],
        }
    }

    pub fn intern(&mut self, value: &T) -> Interned<T> {
        if let Some(&id) = self.map.get(value) {
            return Interned {
                slice_id: id,
                marker: PhantomData,
            };
        }
        let (slice, interned) = unsafe { self.buffer(value) };
        let id = self.slices.len();
        self.map.insert(interned, id);
        self.slices.push(slice);
        Interned {
            slice_id: id,
            marker: PhantomData,
        }
    }

    pub fn get(&self, interned: &Interned<T>) -> Option<&T> {
        self.slices
            .get(interned.slice_id)
            .map(|&slice| T::from_bytes(slice))
    }

    // Safety: We preserve pointer validity by chaining buffers together
    //   rather than re-allocating them
    unsafe fn buffer(&mut self, value: &T) -> (&'static [u8], &'static T) {
        let bytes = value.to_bytes();
        let buffer = self.buffers.last().unwrap();
        let capacity = buffer.capacity();
        if capacity < buffer.len() + bytes.len() {
            let new_capacity = (capacity.max(bytes.len()) + 1).next_power_of_two();
            self.buffers.push(Vec::with_capacity(new_capacity));
        }

        let buffer = self.buffers.last_mut().unwrap();
        let start = buffer.len();
        buffer.extend_from_slice(bytes);

        // Here's the sketchy bit. We coerce the newly copied bytes into a slice
        let slice = &*slice_from_raw_parts(buffer.as_ptr().add(start), bytes.len());
        (slice, T::from_bytes(slice))
    }
}

pub trait InternerRcExt<T>
where
    T: Internable + Eq + Hash + ?Sized + 'static,
{
    fn intern(&mut self, value: &T) -> Option<Interned<T>>;
}

impl<T> InternerRcExt<T> for Rc<Interner<T>>
where
    T: Internable + Eq + Hash + ?Sized + 'static,
{
    fn intern(&mut self, value: &T) -> Option<Interned<T>> {
        Rc::get_mut(self).map(|s| s.intern(value))
    }
}

impl<T> InternerRcExt<T> for Arc<Interner<T>>
where
    T: Internable + Eq + Hash + ?Sized + 'static,
{
    fn intern(&mut self, value: &T) -> Option<Interned<T>> {
        Arc::get_mut(self).map(|s| s.intern(value))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        let mut int = Interner::new();
        let hello = int.intern("hello");

        assert_eq!("hello", int.get(&hello).unwrap());

        let shoes = int.intern("shoes");

        assert_eq!("hello", int.get(&hello).unwrap());
        assert_eq!("shoes", int.get(&shoes).unwrap());

        let socks = int.intern("socks");

        assert_eq!("hello", int.get(&hello).unwrap());
        assert_eq!("shoes", int.get(&shoes).unwrap());
        assert_eq!("socks", int.get(&socks).unwrap());

        let shirts = int.intern("shirts");

        assert_eq!("hello", int.get(&hello).unwrap());
        assert_eq!("shoes", int.get(&shoes).unwrap());
        assert_eq!("socks", int.get(&socks).unwrap());
        assert_eq!("shirts", int.get(&shirts).unwrap());
    }
}
