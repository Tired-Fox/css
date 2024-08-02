use wml::*;

use std::{borrow::Cow, fmt::Display, io::Write, path::PathBuf};
use serde::Deserialize;

pub fn get_tests(root: &'static str) -> Vec<PathBuf> {
    println!("\x1b[1mRunning tests for \x1b[35m{}\x1b[0m", root.to_ascii_uppercase());
    std::fs::read_dir(format!("tests/{root}"))
        .unwrap()
        .filter_map(|v| v.ok())
        .filter(|v| v.path().is_file() && v.path().extension().map(|v| v == "json").unwrap_or_default())
        .map(|v| v.path())
        .collect::<Vec<_>>()
}

#[derive(Debug, Deserialize)]
struct TokenizerTest {
    name: Cow<'static, str>,
    input: Cow<'static, str>,
    output: Vec<Token>,
    #[serde(default)]
    parse_errors: bool,
    #[serde(default)]
    capture_comments: bool,
}

#[derive(Clone)]
struct Fail {
    message: String,
    expected: String,
    actual: String,
}

impl Display for Fail {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "  \x1b[2;31m{}\x1b[22;39m\n      \x1b[2mexpected: {}\x1b[22m\n      \x1b[2mactual: {}\x1b[22m", self.message, self.expected, self.actual)
    }
}

impl Fail {
    pub fn new<S1, S2, S3>(expected: S1, actual: S2, message: S3) -> Self
    where
        S1: Display,
        S2: Display,
        S3: Display,
    {
        Self {
            expected: expected.to_string(),
            actual: actual.to_string(),
            message: message.to_string(),
        }
    }
}

fn run_tokenization_tests() {
    println!("\x1b[1mRunning tests for \x1b[35mTokenization\x1b[0m");
    let data = std::fs::read_to_string("tests/tokenize.json").unwrap();
    let tests = serde_json::from_str::<Vec<TokenizerTest>>(data.as_str()).unwrap();
    for test in tests {
        print!("  \x1b[2m{}\x1b[0m  ...  ", test.name);
        std::io::stdout().flush().unwrap();

        let mut tokenizer = Tokenizer::new(test.input.bytes(), TokenizerOptions::default().capture_comments(test.capture_comments));
        let mut tokens = Vec::new();
        for token in tokenizer.by_ref() {
            if matches!(token , Token::EOF) {
                break;
            }
            tokens.push(token);
        }

        let mut fails: Vec<Fail> = Vec::new();
        let mut pass = true;

        if tokens.len() != test.output.len() {
            fails.push(Fail::new(
                format!("\x1b[33m{}\x1b[39m", test.output.len()),
                format!("\x1b[34m{}\x1b[39m", tokens.len()),
                "Tokenizer did not produce enough tokens"
            ));
            pass = false;
        }

        if pass {
            for (i, (actual, expected)) in tokens.iter().zip(test.output.iter()).enumerate() {
                if actual.as_ref() != expected.as_ref() {
                    fails.push(Fail::new(
                        format!("\x1b[33m{}\x1b[39m", expected.as_ref()),
                        format!("\x1b[31m{}\x1b[39m", actual.as_ref()),
                        format!("Mismatched tokens at index {i}")
                    ));
                    pass = false;
                } else if actual != expected {
                    fails.push(Fail::new(
                        format!("\x1b[33m{:?}\x1b[39m", expected),
                        format!("\x1b[31m{:?}\x1b[39m", actual),
                        format!("Invalid value of token at index {i}")
                    ));
                    pass = false;
                }
            }
        }

        if pass && test.parse_errors != tokenizer.has_errors() {
            fails.push(Fail::new(
                format!("\x1b[33m{}\x1b[39m", if test.parse_errors { "multiple"} else { "none" }),
                format!("\x1b[2m[{}]\x1b[22m", tokenizer.errors().iter().map(|v| format!("\x1b[31m'{v}'\x1b[39m")).collect::<Vec<_>>().join(",")),
                format!("Expected {}", if test.parse_errors { "parse errors"} else { "no parse errors" })
            ));
            pass = false;
        }

        println!("{}", if pass { "\x1b[32mPASS\x1b[39m" } else { "\x1b[31mFAIL\x1b[39m" });
        for fail in fails.iter() {
            println!("{}", fail);
        }
    }
}

fn main() {
    run_tokenization_tests();
}
