use futures_util::StreamExt;
use wml::*;

use std::{borrow::Cow, io::Write, path::PathBuf};
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
struct Test {
    name: Cow<'static, str>,
    input: Cow<'static, str>,
    output: Vec<Token>,
    parse_errors: bool
}

fn run_tokenization_tests() {
    for test in get_tests("tokenize") {
        println!("  \x1b[1;33m{}\x1b[0m", match test.file_stem() {
            Some(stem) => stem.to_str().unwrap().to_ascii_uppercase(),
            None => String::new()
        });
        let data = std::fs::read_to_string(&test).unwrap();
        let tests = serde_json::from_str::<Vec<Test>>(data.as_str()).unwrap();
        for test in tests {
            print!("    \x1b[2m{}\x1b[0m  ...  ", test.name);
            std::io::stdout().flush().unwrap();

            let mut tokenizer = Tokenizer::new(test.input.bytes(), TokenizerOptions::default());
            let mut tokens = Vec::new();
            for token in tokenizer.by_ref() {
                if matches!(token , Token::EOF) {
                    break;
                }
                tokens.push(token);
            }

            let mut pass = tokens == test.output;
            if test.parse_errors && !tokenizer.has_errors() {
                pass = false;
            }
            print!("\x1b[1;{}mSYNC\x1b[0m", if pass { 32 } else { 31 });

            let input = test.input.clone();
            #[cfg(feature = "async")]
            {
                smol::block_on(async move {
                    let mut tokenizer = AsyncTokenizer::new(futures_util::stream::iter(input.bytes()), TokenizerOptions::default());
                    let mut tokens = Vec::new();
                    while let Some(token) = tokenizer.next().await {
                        if matches!(token , Token::EOF) {
                            break;
                        }
                        tokens.push(token);
                    }

                    let mut pass = tokens == test.output;
                    if test.parse_errors && !tokenizer.has_errors() {
                        pass = false;
                    }
                    print!(", \x1b[1;{}mASYNC\x1b[0m", if pass { 32 } else { 31 });
                })
            }
            println!();
        }
    }
}

fn main() {
    run_tokenization_tests();
}
