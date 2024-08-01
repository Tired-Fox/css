use hashbrown::HashSet;
use std::{borrow::Cow, fmt::Display};

use encoding_rs::Encoding;

use super::code_point::CodePointStream;

// Option condition on some
macro_rules! oc {
    ($ident: ident; $cond: expr) => {
        $ident.map(|$ident| $cond).unwrap_or_default() 
    };
}

pub trait TokenizeCharHelper {
    fn is_ident_start(self) -> bool;
    fn non_printable(self) -> bool;
    fn is_ident(self) -> bool;
}

impl TokenizeCharHelper for char {
    fn is_ident_start(self) -> bool {
        self.is_ascii_alphabetic() || self == '_' || self as u32 >= 0x0080
    }

    fn non_printable(self) -> bool {
        let val = self as u32;
        val == 0x000B
            || val == 0x007F
            || (0x0000..=0x0008).contains(&val)
            || (0x000E..=0x001F).contains(&val)
    }

    fn is_ident(self) -> bool {
        self.is_ident_start() || self.is_ascii_digit() || self == '-'
    }
}

#[derive(Default, Debug, Clone, PartialEq, strum_macros::EnumIs)]
enum NumberType {
    #[default]
    Integer,
    Number,
}

#[derive(Debug, Clone, PartialEq, strum_macros::EnumIs)]
pub enum NumberValue {
    Integer(i32),
    Number(f32)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Number {
    repr: Cow<'static, str>,
    value: NumberValue,
}

#[derive(Default, Debug, Clone, PartialEq, strum_macros::EnumIs)]
pub enum HashType {
    Id,
    #[default]
    Unrestricted,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, strum_macros::EnumIs)]
pub enum Token {
    Ident(Cow<'static, str>),
    Function(Cow<'static, str>),
    AtKeyword(Cow<'static, str>),

    Hash(Cow<'static, str>, HashType),
    String(Cow<'static, str>),
    Url(Cow<'static, str>),
    BadString,
    BadUrl,

    Comment(Cow<'static, str>),
    Whitespace(String),

    Number(Number),
    Percentage(Number),
    Dimension { value: Number, unit: Cow<'static, str> },

    CDO,
    CDC,
    Delim(char),

    Colon,
    Semicolon,
    Comma,
    LeftBrace,
    RightBrace,
    LeftParenthesis,
    RightParenthesis,
    LeftBracket,
    RightBracket,

    #[allow(clippy::upper_case_acronyms)]
    EOF,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let repr = match self {
            Self::Ident(value) => value.to_string(),
            Self::Function(name) => format!("{name}("),
            Self::AtKeyword(keyword) => format!("@{keyword}"),
            Self::Hash(hash, _) => hash.to_string(),
            Self::String(value) => value.to_string(),
            Self::Url(url) => format!("url({url})"),
            Self::BadString => "/* Bad String */".to_string(),
            Self::BadUrl => "/* Bad Url */".to_string(),
            Self::Comment(comment) => format!("/*{comment}*/"),
            Self::Whitespace(whitespace) => whitespace.clone(),
            Self::Number(Number { repr, .. }) => repr.to_string(),
            Self::Percentage(Number { repr, .. }) => format!("{repr}%"),
            Self::Dimension { value: Number { repr, .. }, unit } => format!("{repr}{unit}"),
            Self::CDO => "<!--".to_string(), 
            Self::CDC => "-->".to_string(),
            Self::Delim(char) => char.to_string(),
            Self::Colon => ':'.to_string(),
            Self::Semicolon => ';'.to_string(),
            Self::Comma => ','.to_string(),
            Self::LeftBrace => '['.to_string(),
            Self::RightBrace => ']'.to_string(),
            Self::LeftParenthesis => '('.to_string(),
            Self::RightParenthesis => ')'.to_string(),
            Self::LeftBracket => '{'.to_string(),
            Self::RightBracket => '}'.to_string(),
            Self::EOF => String::new()
        };

        write!(f, "{}", repr)
    }
}

#[derive(Debug, Clone, Default)]
pub struct TokenizerOptions {
    encoding: Option<&'static Encoding>,
    capture_comments: bool,
}

/// A CSS tokenizer.
///
/// This tokenizer is a stateful stream of tokens. The common text tokens such as strings, urls,
/// identifiers, etc. are cached and reused if it occurs more than once.
pub struct Tokenizer {
    options: TokenizerOptions,

    cache: HashSet<Cow<'static, str>>,
    parse_errors: Vec<String>,
    input: CodePointStream,
}

impl Tokenizer {
    /// Create a new tokenizer from a byte iterator and options.
    pub fn new<I: IntoIterator<Item = u8> + 'static>(input: I, options: TokenizerOptions) -> Self {
        Self {
            input: CodePointStream::new(input, options.encoding),
            cache: HashSet::new(),
            parse_errors: Vec::new(),
            options,
        }
    }

    /// Check if the provided two code points are a valid escape sequence.
    #[inline]
    pub fn is_valid_escape(code_points: [Option<char>; 2]) -> bool {
        let [first, second] = code_points;
        oc!(first; first == '\\') && oc!(second; second != '\n')
    }

    /// Check if the provided three code points are a valid start of an identifier.
    #[inline]
    pub fn is_start_ident(code_points: [Option<char>; 3]) -> bool {
        let [first, second, third] = code_points;
        if oc!(first; first.is_ident_start()) {
            return true;
        }
        if oc!(first; first == '-') {
            return oc!(second; second.is_ident_start()) || oc!(second; second == '-') || Self::is_valid_escape([second, third]);
        }
        if oc!(first; first == '\\') {
            return Self::is_valid_escape([first, second]);
        }
        false
    }

    /// Check if the provided three code points are a valid start of a number.
    #[inline]
    pub fn is_start_number(code_points: [Option<char>; 3]) -> bool {
        let [first, second, third] = code_points;
        if oc!(first; "+-".contains(first)) {
            return oc!(second; second.is_ascii_digit()) || (oc!(second; second == '.') && oc!(third; third.is_ascii_digit()));
        }

        if oc!(first; first == '.') {
            return oc!(second; second.is_ascii_digit());
        }

        oc!(first; first.is_ascii_digit())
    }

    /// Check if the next two code points are a valid escape sequence from the input stream.
    #[inline]
    pub fn has_valid_escape(&mut self) -> bool {
        Self::is_valid_escape(self.input.peekn::<2>())
    }

    /// Check if the next thre code points are a valid start of an identifier from the input stream.
    #[inline]
    pub fn has_start_ident(&mut self) -> bool {
        Self::is_start_ident(self.input.peekn::<3>())
    }

    /// Check if the next three code points are a valid start of a number from the input stream.
    #[inline]
    pub fn has_start_number(&mut self) -> bool {
        Self::is_start_number(self.input.peekn::<3>())
    }

    /// If the stream starts with `/*` consume all code points until `*/` is found.
    /// This forms a comment token. Comment tokens are only captured and returned if the `capture_comments`
    /// option is set.
    pub fn consume_comment(&mut self) -> Option<Token> {
        if let [Some('/'), Some('*')] = self.input.peekn::<2>() {
            let _ = unsafe { self.input.nextn_unchecked::<2>() };
            let mut comment = String::new();
            loop {
                match self.input.peekn::<2>() {
                    [Some('*'), Some('/')] => {
                        unsafe { self.input.nextn_unchecked::<2>() };
                        break;
                    }
                    [Some(_), _] => comment.push(unsafe { self.input.next_unchecked() }),
                    _ => {
                        // Consume remaining single code point if it exists
                        self.input.next();
                        self.parse_errors.push("unterminated comment".to_string());
                        break;
                    }
                }
            }
            if self.options.capture_comments {
                let comment = self.cache.get_or_insert(Cow::from(comment));
                return Some(Token::Comment(comment.clone()));
            }
        }
        None
    }

    /// If the stream starts with whitespace, consume all whitespace and return a whitespace token.
    pub fn consume_whitespace(&mut self) -> Token {
        let mut whitespace = String::new();
        loop {
            match self.input.peek() {
                Some(c) if c.is_whitespace() => {
                    whitespace.push(unsafe { self.input.next_unchecked() });
                }
                _ => break,
            }
        }
        Token::Whitespace(whitespace)
    }

    /// Assume that the `\` character has been consumed and that the next code point is the start
    /// of a valid escape sequence.
    pub fn consume_escape(&mut self) -> char {
        match self.input.next() {
            Some(c) if c.is_ascii_hexdigit() => {
                let mut hex = String::from(c);

                while let Some(c) = self.input.peek() {
                    if hex.len() >= 6 || !c.is_ascii_hexdigit() {
                        break;
                    }
                    hex.push(unsafe { self.input.next_unchecked() });
                }

                if let Some(' ') = self.input.peek() {
                    unsafe { self.input.next_unchecked() };
                }

                let hex = u32::from_str_radix(hex.as_str(), 16).unwrap();
                if hex > 0x10FFFF || hex == 0 {
                    return char::REPLACEMENT_CHARACTER;
                }
                char::from_u32(hex).unwrap_or(char::REPLACEMENT_CHARACTER)
            }
            None => {
                self.parse_errors
                    .push("unterminated escape sequence".to_string());
                char::REPLACEMENT_CHARACTER
            }
            Some(c) => c,
        }
    }

    /// Assume that the opening code point has already been parsed and is passed in as `opening`.
    ///
    /// This function accepts an optional `ending` code point. If it is not provided, the `opening`
    /// code point will be used as the ending code point.
    pub fn consume_string(&mut self, opening: char, ending: Option<char>) -> Token {
        let ending = ending.unwrap_or(opening);
        let mut string = String::new();
        loop {
            match self.input.peek() {
                Some(c) if c == ending => {
                    unsafe { self.input.next_unchecked() };
                    break;
                }
                None => {
                    self.parse_errors.push("unterminated string".to_string());
                    break;
                }
                Some('\n') => {
                    self.parse_errors.push("unterminated string".to_string());
                    return Token::BadString;
                }
                Some('\\') => {
                    self.input.next();
                    if self.input.peek().is_none() {
                        continue;
                    } else if let Some('\n') = self.input.peek() {
                        string.push(unsafe { self.input.next_unchecked() });
                    } else {
                        string.push(self.consume_escape());
                    }
                }
                Some(_) => string.push(unsafe { self.input.next_unchecked() }),
            }
        }

        let string = self.cache.get_or_insert(Cow::from(string));
        Token::String(string.clone())
    }

    /// Consume an identifier from the input stream and cache it's value. Return a shared reference
    /// to the cached value.
    pub fn consume_identifier(&mut self) -> Cow<'static, str> {
        let mut result = String::new();
        loop {
            match self.input.peek() {
                Some(c) if c.is_ident() => result.push(unsafe { self.input.next_unchecked() }),
                Some(_) if self.has_valid_escape() => {
                    // Pop the `\\` code point
                    unsafe { self.input.next_unchecked() };
                    result.push(self.consume_escape())
                },
                _ => {
                    let result = self.cache.get_or_insert(Cow::from(result));
                    return result.clone();
                }
            }
        }
    }

    /// Consume a number from the input stream and return it.
    ///
    /// Returns a numeric value and a type of either number or integer.
    pub fn consume_number(&mut self) -> Number {
        let mut repr = String::new();
        let mut ntype = NumberType::default();

        if let Some('-' | '+') = self.input.peek() {
            repr.push(unsafe { self.input.next_unchecked() });
        }

        while let Some(c) = self.input.peek() {
            if !c.is_ascii_digit() {
                break;
            }
            repr.push(unsafe { self.input.next_unchecked() });
        }

        // Parse decimal
        if let [Some('.'), Some(c)] = self.input.peekn::<2>() {
            if c.is_ascii_digit() {
                repr.extend(unsafe { self.input.nextn_unchecked::<2>() });
                ntype = NumberType::Number;
                while let Some(c) = self.input.peek() {
                    if !c.is_ascii_digit() {
                        break;
                    }
                    repr.push(unsafe { self.input.next_unchecked() });
                }
            }
        }

        // Parse exponent
        if let [Some('E' | 'e'), Some('+' | '-'), Some(c)] = self.input.peekn::<3>() {
            if c.is_ascii_digit() {
                repr.extend(unsafe { self.input.nextn_unchecked::<3>() });
                ntype = NumberType::Number;
                while let Some(c) = self.input.peek() {
                    if !c.is_ascii_digit() {
                        break;
                    }
                    repr.push(unsafe { self.input.next_unchecked() });
                }
            }
        }

        let value = self.cache.get_or_insert(Cow::from(repr));
        Number {
            value: match ntype {
                NumberType::Integer => NumberValue::Integer(value.parse().unwrap_or(0)),
                NumberType::Number => NumberValue::Number(value.parse().unwrap_or(0.0)),
            },
            repr: value.clone(),
        }
    }

    /// Consume a number token from the input stream and return it if it exists.
    ///
    /// Can return a [`Token::Number`], [`Token::Percentage`], or [`Token::Dimension`].
    pub fn consume_numeric(&mut self) -> Token {
        let number = self.consume_number();
        if self.has_start_ident() {
            return Token::Dimension { value: number, unit: self.consume_identifier() };
        } else if let Some('%') = self.input.peek() {
            unsafe { self.input.next_unchecked() };
            return Token::Percentage(number);
        }
        Token::Number(number)
    }
    
    /// Consume a bad url from the input stream. This just progresses the stream to a recoverable
    /// state/position.
    ///
    /// Returns a [`Token::BadUrl`].
    pub fn consume_bad_url(&mut self) -> Token {
        loop {
            match self.input.peek() {
                Some(')') | None => return Token::BadUrl,
                Some(_) if self.has_valid_escape() => {
                    // Pop the `\\` code point
                    unsafe { self.input.next_unchecked() };
                    let _ = self.consume_escape();
                },
                _ => {}
            }
        }
    }

    /// Consume a url from the input stream and return it if it exists.
    ///
    /// Can return a [`Token::Url`], or [`Token::BadUrl`].
    pub fn consume_url(&mut self) -> Token {
        let mut value = String::new();
        let _ = self.consume_whitespace();
        loop {
            match self.input.peek() {
                Some(')') => {
                    unsafe { self.input.next_unchecked() };
                    return Token::Url(self.cache.get_or_insert(Cow::from(value)).clone())
                },
                Some(' ' | '\t' | '\n') => {
                    while let Some(' ' | '\t' | '\n') = self.input.peek() {
                        unsafe { self.input.next_unchecked() };
                    }

                    match self.input.peek() {
                        Some(')') => {
                            unsafe { self.input.next_unchecked() };
                            return Token::Url(self.cache.get_or_insert(Cow::from(value)).clone())
                        },
                        None => return Token::Url(self.cache.get_or_insert(Cow::from(value)).clone()),
                        _ => return self.consume_bad_url(),
                    }
                }
                Some('"' | '\'' | '(') => {
                    self.parse_errors.push("unterminated url".to_string());
                    return self.consume_bad_url();
                }
                Some(c) if c.non_printable() => {
                    self.parse_errors.push("unterminated url".to_string());
                    return self.consume_bad_url();
                }
                Some('\\') => if self.has_valid_escape() {
                    // Pop the `\\` code point
                    unsafe { self.input.next_unchecked() };
                    value.push(self.consume_escape());
                } else {
                    self.parse_errors.push("unterminated url".to_string());
                    return self.consume_bad_url();
                }
                Some(_) => value.push(unsafe { self.input.next_unchecked() }),
                None => {
                    self.parse_errors.push("unterminated url".to_string());
                    return Token::Url(self.cache.get_or_insert(Cow::from(value)).clone());
                },
            }
        }
    }

    /// Consume an identifier-like token from the input stream and return it if it exists.
    ///
    /// Can return a [`Token::Ident`], [`Token::Function`], [`Token::Url`], or [`Token::BadUrl`].
    pub fn consume_ident_like(&mut self) -> Token {
        let string = self.consume_identifier();
        if string == "url" {
            if let Some('(') = self.input.peek() {
                unsafe { self.input.next_unchecked() };
            }

            while let [Some(' '|'\t'|'\n'), Some(' '|'\t'|'\n')] = self.input.peekn::<2>() {
                unsafe { self.input.nextn_unchecked::<2>() };
            }

            if let Some('\'' | '"') = self.input.peek() {
                Token::Function(string.clone())
            } else if let [Some('"' | '\''), Some(_)] = self.input.peekn::<2>() {
                Token::Function(string.clone())
            } else if let [Some(' '), Some('"' | '\'')] = self.input.peekn::<2>() {
                Token::Function(string.clone())
            } else {
                self.consume_url()
            }
        } else if let Some('(') = self.input.peek() {
            Token::Function(string.clone())
        } else {
            Token::Ident(string.clone())
        }
    }

    /// Consume the next token and return it if it exists.
    pub fn consume(&mut self) -> Token {
        if let Some(c) = self.consume_comment() {
            return c;
        }

        let next = self.input.peek();
        match next {
            None => Token::EOF,
            Some('(') => {
                unsafe { self.input.next_unchecked() };
                Token::LeftParenthesis
            }
            Some(')') => {
                unsafe { self.input.next_unchecked() };
                Token::RightParenthesis
            }
            Some('{') => {
                unsafe { self.input.next_unchecked() };
                Token::LeftBracket
            }
            Some('}') => {
                unsafe { self.input.next_unchecked() };
                Token::RightBracket
            }
            Some('[') => {
                unsafe { self.input.next_unchecked() };
                Token::LeftBrace
            }
            Some(']') => {
                unsafe { self.input.next_unchecked() };
                Token::RightBrace
            }
            Some(':') => {
                unsafe { self.input.next_unchecked() };
                Token::Colon
            }
            Some(';') => {
                unsafe { self.input.next_unchecked() };
                Token::Semicolon
            }
            Some(',') => {
                unsafe { self.input.next_unchecked() };
                Token::Comma
            }
            Some(' ' | '\t' | '\n') => self.consume_whitespace(),
            Some('"') => {
                unsafe { self.input.next_unchecked() };
                self.consume_string('"', None)
            }
            Some('\'') => {
                unsafe { self.input.next_unchecked() };
                self.consume_string('\'', None)
            }
            Some('#') => {
                unsafe { self.input.next_unchecked() };
                match self.input.peek() {
                    Some(c) if c.is_ident() => {
                        let mut hash_type = HashType::default();

                        if self.has_start_ident() {
                            hash_type = HashType::Id;
                        }

                        Token::Hash(self.consume_identifier(), hash_type)
                    }
                    _ => Token::Delim('#'),
                }
            },
            Some('+') => {
                if self.has_start_number() {
                    return self.consume_numeric();
                }
                Token::Delim('+')
            }
            Some('-') => {
                if self.has_start_number() {
                    return self.consume_numeric();
                } else if let [Some('-'), Some('-'), Some('>')] = self.input.peekn::<3>() {
                    unsafe { self.input.nextn_unchecked::<3>() };
                    return Token::CDC;
                } else if self.has_start_ident() {
                    return self.consume_ident_like();
                }
                Token::Delim('-')
            }
            Some('.') => if self.has_start_number() {
                self.consume_numeric()
            } else {
                unsafe { self.input.next_unchecked() };
                Token::Delim('.')
            }
            Some('<') => {
                unsafe { self.input.next_unchecked() };
                if let [Some('!'), Some('-'), Some('-')] = self.input.peekn::<3>() {
                    unsafe { self.input.nextn_unchecked::<3>() };
                    return Token::CDO;
                }
                Token::Delim('<')
            }
            Some('@') => {
                unsafe { self.input.next_unchecked() };
                if self.has_start_ident() {
                    return Token::AtKeyword(self.consume_identifier());
                }
                Token::Delim('@')
            } 
            Some('\\') => {
                if self.has_valid_escape() {
                    return self.consume_ident_like();
                }
                self.parse_errors.push("unterminated escape sequence".to_string());
                Token::Delim('\\')
            }
            Some(c) => {
                if c.is_ident_start() {
                    return self.consume_ident_like()
                } else if c.is_ascii_digit() {
                    return self.consume_numeric();
                }
                Token::Delim(unsafe { self.input.next_unchecked() })
            }
        }
    }
}

impl Iterator for Tokenizer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.consume() {
            Token::EOF => None,
            other => Some(other)
        }
    }
}

impl FromIterator<Token> for String {
    fn from_iter<T: IntoIterator<Item = Token>>(iter: T) -> Self {
        String::from_iter(iter.into_iter().map(|v| v.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_eof() {
        let mut tokenizer = Tokenizer::new("".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::EOF);
    }

    #[test]
    fn parse_comment() {
        let mut tokenizer = Tokenizer::new("/* hello */".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::EOF);

        let mut tokenizer = Tokenizer::new("/* hello ".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::EOF);
        assert!(!tokenizer.parse_errors.is_empty());

        let mut tokenizer = Tokenizer::new(
            "/* hello */".bytes(),
            TokenizerOptions {
                capture_comments: true,
                ..Default::default()
            },
        );
        assert_eq!(
            tokenizer.consume(),
            Token::Comment(Cow::from(" hello "))
        );
    }

    #[test]
    fn parse_whitespace() {
        let mut tokenizer = Tokenizer::new(" \t\n".bytes(), TokenizerOptions::default());
        assert_eq!(
            tokenizer.consume(),
            Token::Whitespace(" \t\n".to_string())
        );
        assert_eq!(tokenizer.consume(), Token::EOF);
    }

    #[test]
    fn parse_string() {
        let mut tokenizer =
            Tokenizer::new(r#""some string\"""#.bytes(), TokenizerOptions::default());
        assert_eq!(
            tokenizer.consume(),
            Token::String(Cow::from(r#"some string""#))
        );
        assert_eq!(tokenizer.consume(), Token::EOF);

        let mut tokenizer =
            Tokenizer::new(r#"'some string\''"#.bytes(), TokenizerOptions::default());
        assert_eq!(
            tokenizer.consume(),
            Token::String(Cow::from(r#"some string'"#))
        );
        assert_eq!(tokenizer.consume(), Token::EOF);
    }

    #[test]
    fn parse_char_tokens() {
        let mut tokenizer = Tokenizer::new("()[]{}:;,".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::LeftParenthesis);
        assert_eq!(tokenizer.consume(), Token::RightParenthesis);
        assert_eq!(tokenizer.consume(), Token::LeftBrace);
        assert_eq!(tokenizer.consume(), Token::RightBrace);
        assert_eq!(tokenizer.consume(), Token::LeftBracket);
        assert_eq!(tokenizer.consume(), Token::RightBracket);
        assert_eq!(tokenizer.consume(), Token::Colon);
        assert_eq!(tokenizer.consume(), Token::Semicolon);
        assert_eq!(tokenizer.consume(), Token::Comma);
        assert_eq!(tokenizer.consume(), Token::EOF);
    }

    #[test]
    fn parse_hash() {
        let mut tokenizer = Tokenizer::new("#".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Delim('#'));

        let mut tokenizer = Tokenizer::new("#1".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Hash(Cow::from("1"), HashType::Unrestricted));

        let mut tokenizer = Tokenizer::new("#1FB".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Hash(Cow::from("1FB"), HashType::Unrestricted));

        let mut tokenizer = Tokenizer::new("#A2_B".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Hash(Cow::from("A2_B"), HashType::Id));
        let mut tokenizer = Tokenizer::new("#_FB".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Hash(Cow::from("_FB"), HashType::Id));
    }

    #[test]
    fn parse_number() {
        let mut tokenizer = Tokenizer::new("+".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Delim('+'));

        let mut tokenizer = Tokenizer::new("+1".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Number(Number { repr: Cow::from("+1"), value: NumberValue::Integer(1) }));

        let mut tokenizer = Tokenizer::new("+1%".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Percentage(Number { repr: Cow::from("+1"), value: NumberValue::Integer(1) }));

        let mut tokenizer = Tokenizer::new("+1px".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Dimension { value: Number { repr: Cow::from("+1"), value: NumberValue::Integer(1) }, unit: Cow::from("px") });

        let mut tokenizer = Tokenizer::new("+1em".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Dimension { value: Number { repr: Cow::from("+1"), value: NumberValue::Integer(1) }, unit: Cow::from("em") });

        let mut tokenizer = Tokenizer::new("+1e+10em".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Dimension { value: Number { repr: Cow::from("+1e+10"), value: NumberValue::Number(1e+10) }, unit: Cow::from("em") });

        let mut tokenizer = Tokenizer::new("-".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Delim('-'));

        let mut tokenizer = Tokenizer::new("-1".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Number(Number { repr: Cow::from("-1"), value: NumberValue::Integer(-1) }));

        let mut tokenizer = Tokenizer::new("-1%".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Percentage(Number { repr: Cow::from("-1"), value: NumberValue::Integer(-1) }));

        let mut tokenizer = Tokenizer::new("-1px".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Dimension { value: Number { repr: Cow::from("-1"), value: NumberValue::Integer(-1) }, unit: Cow::from("px") });

        let mut tokenizer = Tokenizer::new("-1em".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Dimension { value: Number { repr: Cow::from("-1"), value: NumberValue::Integer(-1) }, unit: Cow::from("em") });

        let mut tokenizer = Tokenizer::new("-.1e-10em".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Dimension { value: Number { repr: Cow::from("-.1e-10"), value: NumberValue::Number(-0.1e-10) }, unit: Cow::from("em") });

        let mut tokenizer = Tokenizer::new(".".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Delim('.'));

        let mut tokenizer = Tokenizer::new(".1".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Number(Number { repr: Cow::from(".1"), value: NumberValue::Number(0.1) }));

        let mut tokenizer = Tokenizer::new(".1%".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Percentage(Number { repr: Cow::from(".1"), value: NumberValue::Number(0.1) }));

        let mut tokenizer = Tokenizer::new(".1px".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Dimension { value: Number { repr: Cow::from(".1"), value: NumberValue::Number(0.1) }, unit: Cow::from("px") });

        let mut tokenizer = Tokenizer::new(".1em".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Dimension { value: Number { repr: Cow::from(".1"), value: NumberValue::Number(0.1) }, unit: Cow::from("em") });

        let mut tokenizer = Tokenizer::new(".1e+10em".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Dimension { value: Number { repr: Cow::from(".1e+10"), value: NumberValue::Number(0.1e+10) }, unit: Cow::from("em") });

        let mut tokenizer = Tokenizer::new("123".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Number(Number { repr: Cow::from("123"), value: NumberValue::Integer(123) }));

        let mut tokenizer = Tokenizer::new("123%".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Percentage(Number { repr: Cow::from("123"), value: NumberValue::Integer(123) }));

        let mut tokenizer = Tokenizer::new("123px".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Dimension { value: Number { repr: Cow::from("123"), value: NumberValue::Integer(123) }, unit: Cow::from("px") });

        let mut tokenizer = Tokenizer::new("123em".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Dimension { value: Number { repr: Cow::from("123"), value: NumberValue::Integer(123) }, unit: Cow::from("em") });

        let mut tokenizer = Tokenizer::new("123e+10em".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Dimension { value: Number { repr: Cow::from("123e+10"), value: NumberValue::Number(123e+10) }, unit: Cow::from("em") });
    }

    #[test]
    fn dash_ident() {
        let mut tokenizer = Tokenizer::new("-h".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Ident(Cow::from("-h")));

        let mut tokenizer = Tokenizer::new("--".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Ident(Cow::from("--")));

        let mut tokenizer = Tokenizer::new("-\\_".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Ident(Cow::from("-_")));
    }

    #[test]
    fn html_comment() {
        let mut tokenizer = Tokenizer::new("-->".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::CDC);

        let mut tokenizer = Tokenizer::new("<!--".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::CDO);

        let mut tokenizer = Tokenizer::new("<".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Delim('<'));
    }

    #[test]
    fn at_keyword() {
        let mut tokenizer = Tokenizer::new("@".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Delim('@'));

        let mut tokenizer = Tokenizer::new("@import".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::AtKeyword(Cow::from("import")));
    }

    #[test]
    fn ident_like() {
        let mut tokenizer = Tokenizer::new("url(".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Url(Cow::from("")));

        let mut tokenizer = Tokenizer::new("url(https://example.com".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Url(Cow::from("https://example.com")));

        let mut tokenizer = Tokenizer::new("url(https://example.com)".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Url(Cow::from("https://example.com")));

        let mut tokenizer = Tokenizer::new("url(\"https://example.com\")".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Function(Cow::from("url")));

        let mut tokenizer = Tokenizer::new("rgb(128 92 255)".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Function(Cow::from("rgb")));

        let mut tokenizer = Tokenizer::new("hello".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Ident(Cow::from("hello")));

        let mut tokenizer = Tokenizer::new("*".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Delim('*'));
    }

    #[test]
    fn escape() {
        let mut tokenizer = Tokenizer::new("\\unk".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Ident(Cow::from("unk")));

        let mut tokenizer = Tokenizer::new("\\unk(".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Function(Cow::from("unk")));

        let mut tokenizer = Tokenizer::new("\\url(".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::Url(Cow::from("")));

        let mut tokenizer = Tokenizer::new("@import".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.consume(), Token::AtKeyword(Cow::from("import")));
    }


    #[test]
    fn iterate() {
        let tokenizer = Tokenizer::new("/* sample */ url(https://example.com)".bytes(), TokenizerOptions::default());
        assert_eq!(tokenizer.collect::<Vec<Token>>(), vec![
            Token::Whitespace(" ".to_string()),
            Token::Url(Cow::from("https://example.com")),
        ]);
    }
}
