use std::{borrow::Borrow, hash::Hash, marker::PhantomData, mem, ops::{Add, DerefMut}, ptr, rc::Rc, str, sync::{Arc, RwLock}};

use fxhash::FxHashSet;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct BytesRef(RiskySlice<'static>);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct RiskySlice<'a> {
    data: *const u8,
    len: usize,
    marker: PhantomData<&'a ()>,
}

impl<'a> Borrow<[u8]> for RiskySlice<'a> {
    fn borrow(&self) -> &[u8] {
        unsafe { &*ptr::slice_from_raw_parts(self.data, self.len) }
    }
}

#[derive(Default, Debug)]
pub struct BytesInterner {
    map: FxHashSet<RiskySlice<'static>>,
    buffers: Vec<Vec<u8>>,
}

impl BytesInterner {
    #[inline]
    pub fn new() -> Self {
        Self {
            map: FxHashSet::default(),
            buffers: vec![Vec::with_capacity(32)],
        }
    }

    pub fn intern<T: AsRef<[u8]>>(&mut self, bytes: T) -> BytesRef {
        let bytes = bytes.as_ref();
        if let Some(&slice) = self.map.get(bytes) {
            return BytesRef(unsafe { mem::transmute::<_, RiskySlice<'static>>(slice) });
        }
        let slice = self.buffer(bytes);
        self.map.insert(slice);
        BytesRef(slice)
    }

    #[inline]
    pub fn get(&self, bytes: BytesRef) -> Option<&[u8]> {
        self.map.get(&bytes.0).map(|slice| slice.borrow())
    }

    #[inline]
    pub fn eq<T: AsRef<[u8]>>(&self, expected: T, interned: BytesRef) -> Option<bool> {
        self.get(interned).map(|s| expected.as_ref() == s)
    }

    fn buffer(&mut self, slice: &[u8]) -> RiskySlice<'static> {
        let buffer = self.buffers.last().unwrap();
        let capacity = buffer.capacity();
        if capacity < buffer.len() + slice.len() {
            let new_capacity = (capacity.max(slice.len()) + 1).next_power_of_two();
            self.buffers.push(Vec::with_capacity(new_capacity));
        }

        let buffer = self.buffers.last_mut().unwrap();
        let start = buffer.len();
        buffer.extend_from_slice(slice);
        // Safety: We preserve pointer validity by chaining buffers together
        //   rather than re-allocating them
        unsafe {
            RiskySlice {
                data: buffer.as_ptr().add(start),
                len: slice.len(),
                marker: PhantomData,
            }
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct StrRef(BytesRef);

#[derive(Default, Debug)]
pub struct StrInterner {
    inner: BytesInterner,
}

impl StrInterner {
    #[inline]
    pub fn new() -> Self {
        Self {
            inner: BytesInterner::new(),
        }
    }

    #[inline]
    pub fn intern<T: AsRef<str>>(&mut self, string: T) -> StrRef {
        let string = string.as_ref();
        StrRef(self.inner.intern(string.as_bytes()))
    }

    #[inline]
    pub fn get(&self, string: StrRef) -> Option<&str> {
        self.inner
            .get(string.0)
            .map(|b| unsafe { str::from_utf8_unchecked(b) })
    }

    #[inline]
    pub fn eq<T: AsRef<str>>(&self, expected: T, interned: StrRef) -> Option<bool> {
        self.inner.eq(expected.as_ref().as_bytes(), interned.0)
    }
}

pub trait StrInternerRcWriteExt {
    fn intern<T: AsRef<str>>(&mut self, value: T) -> Option<StrRef>;
}

impl StrInternerRcWriteExt for Rc<StrInterner> {
    #[inline]
    fn intern<T: AsRef<str>>(&mut self, value: T) -> Option<StrRef> {
        Rc::get_mut(self).map(|s| s.intern(value))
    }
}

impl StrInternerRcWriteExt for Arc<RwLock<StrInterner>> {
    #[inline]
    fn intern<T: AsRef<str>>(&mut self, value: T) -> Option<StrRef> {
        Arc::get_mut(self).map(|s| s.write().unwrap().deref_mut().intern(value))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sanity() {
        let mut int = StrInterner::new();
        let hello = int.intern("hello");

        assert_eq!("hello", int.get(hello).unwrap());

        let shoes = int.intern("shoes");

        assert_eq!("hello", int.get(hello).unwrap());
        assert_eq!("shoes", int.get(shoes).unwrap());

        let socks = int.intern("socks");

        assert_eq!("hello", int.get(hello).unwrap());
        assert_eq!("shoes", int.get(shoes).unwrap());
        assert_eq!("socks", int.get(socks).unwrap());

        let shirts = int.intern("shirts");

        assert_eq!("hello", int.get(hello).unwrap());
        assert_eq!("shoes", int.get(shoes).unwrap());
        assert_eq!("socks", int.get(socks).unwrap());
        assert_eq!("shirts", int.get(shirts).unwrap());
    }
}
