use std::collections::VecDeque;
use encoding_rs::{CoderResult, Decoder, Encoder, Encoding, UTF_8};

pub const REPLACEMENT_CHARACTER: char = char::REPLACEMENT_CHARACTER;
pub const FORM_FEED: char = 0x000C as char;
pub const NULL: char = 0x0000 as char;

/// Takes a stream of utf-8 bytes and converts them to tokens
pub struct CodePointStream
{
    decoder: Decoder,
    #[allow(dead_code)]
    encoder: Encoder,

    buffer: VecDeque<char>,
    stream: Option<Box<dyn Iterator<Item = u8>>>
}

impl CodePointStream {
    pub const REPLACEMENT_CHARACTER: char = char::REPLACEMENT_CHARACTER;
    pub const FORM_FEED: char = 0x000C as char;
    pub const NULL: char = 0x0000 as char;

    pub fn new<I: IntoIterator<Item = u8> + 'static>(stream: I, encoding: Option<&'static Encoding>) -> Self
    {
        let encoding = encoding.unwrap_or(UTF_8);
        Self {
            decoder: encoding.new_decoder(),
            encoder: encoding.new_encoder(),
            stream: Some(Box::new(stream.into_iter())),
            buffer: VecDeque::new(),
        }
    }

    /// Attempt to read the next char in the stream.
    ///
    /// This means it attempts to read the next 4 bytes. This can result in one unicode char or
    /// 4 ascii chars. If the last code point is a `\r` parse an extra 4 bytes to check for `\r\n`
    ///   pattern.
    pub fn read(&mut self) -> Option<Vec<char>> {
        let mut result = String::new(); 
        
        // get the next 4 bytes. This may result in multiple chars being pushed onto stack, but
        // will be able to handle unicode.
        let mut src = Vec::new();
        for _ in 0..4 {
            if self.stream.is_some() {
                if let Some(point) = self.stream.as_mut().unwrap().next() {
                    src.push(point)
                } else {
                    self.stream = None;
                    break;
                }
            }
        }

        if src.is_empty() {
            return None;
        }

        // If last code point is \r then attempt to read another
        // to check if it is a \r\n pattern
        if let Some(b'\r') = src.last() {
            for _ in 0..4 {
                if self.stream.is_some() {
                    if let Some(point) = self.stream.as_mut().unwrap().next() {
                        src.push(point)
                    } else {
                        self.stream = None;
                        break;
                    }
                }
            }
        }

        // The buffer only holds enough for one unicode to try to enforce 1-4 char reads at once
        let mut index = 0;
        // Read all bytes converting to utf-8 until entire src is converted
        while index < src.len() {
            let mut dest = String::with_capacity(4);
            let (state, read, _replacement) = self.decoder.decode_to_string(&src[index..], &mut dest, false);
            index += read;

            result.push_str(dest.as_str());

            match state {
                CoderResult::InputEmpty => {
                    break
                }
                CoderResult::OutputFull => {}
            }
        }

        Some(result.chars().collect())
    }

    #[inline]
    fn filter_code_points(points: Vec<char>) -> Vec<char> {
        let mut result = Vec::new();
        let mut iter = points.iter().peekable();
        while let Some(next) = iter.next() {
            match next {
                '\r' => {
                    result.push('\n');
                    if let Some('\n') = iter.peek().copied() {
                        let _ = iter.next();
                    }
                },
                // Form Feed
                &FORM_FEED => result.push('\n'),
                &NULL => result.push(REPLACEMENT_CHARACTER),
                other => result.push(*other)
            }
        }
        result
    }

    /// Peek the next code point.
    ///
    /// Per the specification: `\r`, `\f`, and `\r\n` are replaced with `\n`, and U+0000 (NULL) or
    /// surrogate code points are replaced with U+FFFD REPLACEMENT CHARACTER (�)
    #[inline]
    pub fn peek(&mut self) -> Option<char> {
        if self.buffer.is_empty() {
            let chars = self.read()?;
            self.buffer.extend(Self::filter_code_points(chars));
        }
        self.buffer.front().copied()
    }

    /// Peek `N` code points.
    ///
    /// This method will fill the buffer until enough code points exist to peek. If there are not
    /// enough code points, None is returned. Otherwise, the buffer is sliced for the specified `N`
    /// code points returning the set sized array.
    ///
    /// Per the specification: `\r`, `\f`, and `\r\n` are replaced with `\n`, and U+0000 (NULL) or
    /// surrogate code points are replaced with U+FFFD REPLACEMENT CHARACTER (�)
    #[inline]
    pub fn peekn<const N: usize>(&mut self) -> Option<[char;N]> {
        while self.buffer.len() < N {
            let chars = self.read()?;
            self.buffer.extend(Self::filter_code_points(chars));
        }

        if self.buffer.len() >= N {
            let mut result: [char;N] = [Self::NULL;N];
            self.buffer.make_contiguous();
            result.copy_from_slice(&self.buffer.as_slices().0[..N]);
            return Some(result);
        }

        None
    }

    /// Peek the code point at `N`.
    ///
    /// This method will fill the buffer until enough code points exist to peek. If there are not
    /// enough code points, None is returned. Otherwise, the buffer at the specified index is
    /// returned.
    ///
    /// Per the specification: `\r`, `\f`, and `\r\n` are replaced with `\n`, and U+0000 (NULL) or
    /// surrogate code points are replaced with U+FFFD REPLACEMENT CHARACTER (�)
    #[inline]
    pub fn peek_at(&mut self, n: usize) -> Option<char> {
        while self.buffer.len() < n {
            let chars = self.read()?;
            self.buffer.extend(Self::filter_code_points(chars));
        }

        if self.buffer.len() >= n {
            return self.buffer.get(n).copied();
        }

        None
    }

    /// Get the next code point.
    ///
    /// Per the specification: `\r`, `\f`, and `\r\n` are replaced with `\n`, and U+0000 (NULL) or
    /// surrogate code points are replaced with U+FFFD REPLACEMENT CHARACTER (�)
    #[allow(clippy::should_implement_trait)]
    #[inline]
    pub fn next(&mut self) -> Option<char> {
        if self.buffer.is_empty() {
            let chars = self.read()?;
            self.buffer.extend(Self::filter_code_points(chars));
        }
        self.buffer.pop_front()
    }
    
    /// Get the next code point. No bounds checking is performed on the buffer.
    ///
    /// Per the specification: `\r`, `\f`, and `\r\n` are replaced with `\n`, and U+0000 (NULL) or
    /// surrogate code points are replaced with U+FFFD REPLACEMENT CHARACTER (�)
    ///
    /// # Safety
    /// This method assumes that the buffer has at least one code point.
    #[allow(clippy::should_implement_trait)]
    #[inline]
    pub(crate) unsafe fn next_unchecked(&mut self) -> char {
        self.buffer.pop_front().unwrap()
    }

    /// Get the next `N` code points.
    ///
    /// This method will fill the buffer until there are enough code points to return. If there are
    /// not enough code points, None is returned. Otherwise, the buffer is drained for the
    /// specified `N` code points returning the set size array.
    ///
    /// Per the specification: `\r`, `\f`, and `\r\n` are replaced with `\n`, and U+0000 (NULL) or
    /// surrogate code points are replaced with U+FFFD REPLACEMENT CHARACTER (�)
    #[allow(clippy::should_implement_trait)]
    #[inline]
    pub fn nextn<const N: usize>(&mut self) -> Option<[char;N]> {
        while self.buffer.len() < N {
            let chars = self.read()?;
            self.buffer.extend(Self::filter_code_points(chars));
        }

        if self.buffer.len() >= N {
            let mut result: [char;N] = [NULL;N];
            let src = self.buffer.drain(..N).collect::<Vec<_>>();
            result.copy_from_slice(src.as_slice());
            return Some(result);
        }

        None
    }

    /// Get the next `N` code points. No bounds checking is performed on the buffer.
    ///
    /// This method will fill the buffer until there are enough code points to return. If there are
    /// not enough code points, None is returned. Otherwise, the buffer is drained for the
    /// specified `N` code points returning the set size array.
    ///
    /// Per the specification: `\r`, `\f`, and `\r\n` are replaced with `\n`, and U+0000 (NULL) or
    /// surrogate code points are replaced with U+FFFD REPLACEMENT CHARACTER (�)
    ///
    /// # Safety
    /// This method assumes that the buffer has at least `N` code points.
    #[inline]
    pub(crate) unsafe fn nextn_unchecked<const N: usize>(&mut self) -> [char;N] {
        let mut result: [char;N] = [NULL;N];
        let src = self.buffer.drain(..N).collect::<Vec<_>>();
        result.copy_from_slice(src.as_slice());
        result
    }
}

#[cfg(feature="async")]
mod async_code_point {
    use std::{collections::VecDeque, pin::Pin};

    use encoding_rs::{CoderResult, Decoder, Encoder, Encoding, UTF_8};
    use futures_util::{Stream, StreamExt};

    use super::{NULL, FORM_FEED, REPLACEMENT_CHARACTER};

    /// Takes a stream of utf-8 bytes and converts them to tokens
    pub struct AsyncCodePointStream
    {
        decoder: Decoder,
        #[allow(dead_code)]
        encoder: Encoder,

        buffer: VecDeque<char>,
        stream: Pin<Box<dyn Stream<Item = u8>>>,
    }

    impl AsyncCodePointStream {

        pub fn new<I: Stream<Item = u8> + Unpin + 'static>(stream: I, encoding: Option<&'static Encoding>) -> Self {
            let encoding = encoding.unwrap_or(UTF_8);
            Self {
                decoder: encoding.new_decoder(),
                encoder: encoding.new_encoder(),
                stream: Box::pin(stream),
                buffer: VecDeque::new(),
            }
        }

        /// Attempt to read the next char in the stream.
        ///
        /// This means it attempts to read the next 4 bytes. This can result in one unicode char or
        /// 4 ascii chars.
        pub async fn read(&mut self) -> Option<Vec<char>> {
            let mut result = String::new(); 
            
            // get the next 4 bytes. This may result in multiple chars being pushed onto stack, but
            // will be able to handle unicode.
            let mut src = Vec::new();
            for _ in 0..4 {
                if let Some(point) = self.stream.next().await {
                    src.push(point);
                } else {
                    break;
                }
            }

            if src.is_empty() {
                return None;
            }

            // If last code point is \r then attempt to read another
            // to check if it is a \r\n pattern
            if let Some(b'\r') = src.last() {
                if let Some(point) = self.stream.next().await {
                    src.push(point);
                }
            }


            // The buffer only holds enough for one unicode to try to enforce 1-4 char reads at once
            let mut index = 0;
            // Read all bytes converting to utf-8 until entire src is converted
            while index < src.len() {
                let mut dest = String::with_capacity(5);
                let (state, read, _replacement) = self.decoder.decode_to_string(&src[index..], &mut dest, false);
                index += read;

                result.push_str(dest.as_str());

                match state {
                    CoderResult::InputEmpty => {
                        break
                    }
                    CoderResult::OutputFull => {}
                }
            }

            Some(result.chars().collect())
        }

        fn filter_code_points(points: Vec<char>) -> Vec<char> {
            let mut result = Vec::new();
            let mut iter = points.iter().peekable();
            while let Some(next) = iter.next() {
                match next {
                    '\r' => {
                        result.push('\n');
                        if let Some('\n') = iter.peek().copied() {
                            let _ = iter.next();
                        }
                    },
                    // Form Feed
                    &FORM_FEED => result.push('\n'),
                    &NULL => result.push(REPLACEMENT_CHARACTER),
                    other => result.push(*other)
                }
            }
            result
        }

        /// Peek the next code point.
        ///
        /// Per the specification: `\r`, `\f`, and `\r\n` are replaced with `\n`, and U+0000 (NULL) or
        /// surrogate code points are replaced with U+FFFD REPLACEMENT CHARACTER (�)
        pub async fn peek(&mut self) -> Option<char> {
            if self.buffer.is_empty() {
                let chars = self.read().await?;
                self.buffer.extend(Self::filter_code_points(chars));
            }
            self.buffer.front().copied()
        }

        /// Peek `N` code points.
        ///
        /// This method will fill the buffer until enough code points exist to peek. If there are not
        /// enough code points, None is returned. Otherwise, the buffer is sliced for the specified `N`
        /// code points returning the set sized array.
        ///
        /// Per the specification: `\r`, `\f`, and `\r\n` are replaced with `\n`, and U+0000 (NULL) or
        /// surrogate code points are replaced with U+FFFD REPLACEMENT CHARACTER (�)
        pub async fn peekn<const N: usize>(&mut self) -> Option<[char;N]> {
            while self.buffer.len() < N {
                let chars = self.read().await?;
                self.buffer.extend(Self::filter_code_points(chars));
            }

            if self.buffer.len() >= N {
                let mut result: [char;N] = [NULL;N];
                self.buffer.make_contiguous();
                result.copy_from_slice(&self.buffer.as_slices().0[..N]);
                return Some(result);
            }

            None
        }

        /// Get the next code point.
        ///
        /// Per the specification: `\r`, `\f`, and `\r\n` are replaced with `\n`, and U+0000 (NULL) or
        /// surrogate code points are replaced with U+FFFD REPLACEMENT CHARACTER (�)
        #[allow(clippy::should_implement_trait)]
        pub async fn next(&mut self) -> Option<char> {
            if self.buffer.is_empty() {
                let chars = self.read().await?;
                self.buffer.extend(Self::filter_code_points(chars));
            }
            self.buffer.pop_front()
        }

        /// Get the next `N` code points.
        ///
        /// This method will fill the buffer until there are enough code points to return. If there are
        /// not enough code points, None is returned. Otherwise, the buffer is drained for the
        /// specified `N` code points returning the set size array.
        ///
        /// Per the specification: `\r`, `\f`, and `\r\n` are replaced with `\n`, and U+0000 (NULL) or
        /// surrogate code points are replaced with U+FFFD REPLACEMENT CHARACTER (�)
        #[allow(clippy::should_implement_trait)]
        pub async fn nextn<const N: usize>(&mut self) -> Option<[char;N]> {
            while self.buffer.len() < N {
                let chars = self.read().await?;
                self.buffer.extend(Self::filter_code_points(chars));
            }

            if self.buffer.len() >= N {
                let mut result: [char;N] = [NULL;N];
                let src = self.buffer.drain(..N).collect::<Vec<_>>();
                result.copy_from_slice(src.as_slice());
                return Some(result);
            }

            None
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        macro_rules! stream_from_iter {
            ($($token: tt)*) => {
                futures_util::stream::iter($($token)*.into_iter()) 
            };
        }

        #[test]
        fn read_code_points() {
            smol::block_on(async move {
                let mut stream = AsyncCodePointStream::new(
                    stream_from_iter!([240u8, 159, 153, 130, 240, 159, 0xDBu8, 0xFFu8, 240, 159, 0xD8u8, 0x00u8, 34, 104, 105, 34]),
                    None
                );
                assert_eq!(stream.read().await, Some(vec!['\u{1F642}']));
                assert_eq!(stream.read().await, Some(vec!['�', '�', '�']));
                assert_eq!(stream.read().await, Some(vec!['�', '�', '\0']));
                assert_eq!(stream.read().await, Some(vec!['"', 'h', 'i', '"']));
            })
        }

        #[test]
        fn peek_code_point() {
            smol::block_on(async move {
                let mut stream = AsyncCodePointStream::new(stream_from_iter!("\u{1F642}@import".bytes()), None);
                assert_eq!(stream.peek().await, Some('\u{1F642}'));
                assert!(stream.buffer.len() == 1);

                // Assert that it will not read again if the buffer already has a code point
                assert_eq!(stream.peek().await, Some('\u{1F642}'));
                assert!(stream.buffer.len() == 1);

                // Clear the buffer so a new value can be read
                let _ = stream.buffer.pop_front();
                assert_eq!(stream.peek().await, Some('@'));
                assert!(stream.buffer.len() == 4);

                let _ = stream.buffer.drain(..);
                let _ = stream.read().await;
                // Returns none if nothing is in backlog and cannot read more from stream
                assert!(stream.peek().await.is_none());
            })
        }

        #[test]
        fn next_code_point() {
            smol::block_on(async move {
                let mut stream = AsyncCodePointStream::new(stream_from_iter!("@import".bytes()), None);
                assert_eq!(stream.next().await, Some('@'));
                assert_eq!(stream.buffer.len(), 3);
                assert_eq!(stream.next().await, Some('i'));
                assert_eq!(stream.next().await, Some('m'));
                assert_eq!(stream.next().await, Some('p'));
                assert_eq!(stream.buffer.len(), 0);

                // Should read next chunk of 4 bytes
                assert_eq!(stream.next().await, Some('o'));
                assert_eq!(stream.next().await, Some('r'));
                assert_eq!(stream.next().await, Some('t'));
                assert_eq!(stream.buffer.len(), 0);
                assert!(stream.next().await.is_none());
            })
        }

        #[test]
        fn peek_multiple_code_points() {
            smol::block_on(async move {
                let mut stream = AsyncCodePointStream::new(stream_from_iter!("@import".bytes()), None);
                assert_eq!(stream.peekn::<3>().await, Some(['@', 'i', 'm']));
                assert_eq!(stream.buffer.len(), 4);
                assert_eq!(stream.peekn::<4>().await, Some(['@', 'i', 'm', 'p']));
                assert_eq!(stream.peekn::<5>().await, Some(['@', 'i', 'm', 'p', 'o']));
                assert_eq!(stream.buffer.len(), 7);
                assert_eq!(stream.peekn::<7>().await, Some(['@', 'i', 'm', 'p', 'o', 'r', 't']));
                assert!(stream.peekn::<8>().await.is_none());
            })
        }

        #[test]
        fn next_multiple_code_points() {
            smol::block_on(async move {
                let mut stream = AsyncCodePointStream::new(stream_from_iter!("@import".bytes()), None);
                assert_eq!(stream.nextn::<3>().await, Some(['@', 'i', 'm']));
                assert_eq!(stream.buffer.len(), 1);
                assert_eq!(stream.nextn::<4>().await, Some(['p', 'o', 'r', 't']));
                assert_eq!(stream.buffer.len(), 0);
                assert!(stream.nextn::<2>().await.is_none());
            })
        }
    }
}

#[cfg(feature="async")]
pub use async_code_point::AsyncCodePointStream;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_code_points() {
        let mut stream = CodePointStream::new([240u8, 159, 153, 130, 240, 159, 0xDBu8, 0xFFu8, 240, 159, 0xD8u8, 0x00u8, 34, 104, 105, 34], None);
        assert_eq!(stream.read(), Some(vec!['\u{1F642}']));
        assert_eq!(stream.read(), Some(vec!['�', '�', '�']));
        assert_eq!(stream.read(), Some(vec!['�', '�', '\0']));
        assert_eq!(stream.read(), Some(vec!['"', 'h', 'i', '"']));
    }

    #[test]
    fn peek_code_point() {
        let mut stream = CodePointStream::new("\u{1F642}@import".bytes(), None);
        assert_eq!(stream.peek(), Some('\u{1F642}'));
        assert!(stream.buffer.len() == 1);

        // Assert that it will not read again if the buffer already has a code point
        assert_eq!(stream.peek(), Some('\u{1F642}'));
        assert!(stream.buffer.len() == 1);

        // Clear the buffer so a new value can be read
        let _ = stream.buffer.pop_front();
        assert_eq!(stream.peek(), Some('@'));
        assert!(stream.buffer.len() == 4);

        let _ = stream.buffer.drain(..);
        let _ = stream.read();
        // Returns none if nothing is in backlog and cannot read more from stream
        assert!(stream.peek().is_none());
    }

    #[test]
    fn next_code_point() {
        let mut stream = CodePointStream::new("@import".bytes(), None);
        assert_eq!(stream.next(), Some('@'));
        assert_eq!(stream.buffer.len(), 3);
        assert_eq!(stream.next(), Some('i'));
        assert_eq!(stream.next(), Some('m'));
        assert_eq!(stream.next(), Some('p'));
        assert_eq!(stream.buffer.len(), 0);

        // Should read next chunk of 4 bytes
        assert_eq!(stream.next(), Some('o'));
        assert_eq!(stream.next(), Some('r'));
        assert_eq!(stream.next(), Some('t'));
        assert_eq!(stream.buffer.len(), 0);
        assert!(stream.next().is_none());
    }

    #[test]
    fn peek_multiple_code_points() {
        let mut stream = CodePointStream::new("@import".bytes(), None);
        assert_eq!(stream.peekn::<3>(), Some(['@', 'i', 'm']));
        assert_eq!(stream.buffer.len(), 4);
        assert_eq!(stream.peekn::<4>(), Some(['@', 'i', 'm', 'p']));
        assert_eq!(stream.peekn::<5>(), Some(['@', 'i', 'm', 'p', 'o']));
        assert_eq!(stream.buffer.len(), 7);
        assert_eq!(stream.peekn::<7>(), Some(['@', 'i', 'm', 'p', 'o', 'r', 't']));
        assert!(stream.peekn::<8>().is_none());
    }

    #[test]
    fn next_multiple_code_points() {
        let mut stream = CodePointStream::new("@import".bytes(), None);
        assert_eq!(stream.nextn::<3>(), Some(['@', 'i', 'm']));
        //assert_eq!(stream.buffer.len(), 1);
        //assert_eq!(stream.nextn::<4>(), Some(['p', 'o', 'r', 't']));
        //assert_eq!(stream.buffer.len(), 0);
        //assert!(stream.nextn::<2>().is_none());
    }
}
