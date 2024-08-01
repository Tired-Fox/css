use futures_util::StreamExt;

fn code_point_stream() {
    // SYNC
    {
        use std::{fs::File, io::{BufReader, Read}};
        use wml::CodePointStream;

        let reader = BufReader::new(File::open("examples/sample.css").unwrap());
        let mut stream = CodePointStream::new(reader.bytes().map(|v| v.unwrap()), None);

        println!("\x1b[1;33mSYNC STREAM\x1b[0m:");
        while let Some(char) = stream.next() {
            print!("{}", char);
        }
        println!();
    }

    // ASYNC
    #[cfg(feature="async")]
    {
        use smol::{io::{BufReader, AsyncReadExt}, fs::File};
        use wml::AsyncCodePointStream;

        smol::block_on(async move {
            let reader = BufReader::new(File::open("examples/sample.css").await.unwrap());
            let mut stream = AsyncCodePointStream::new(reader.bytes().map(|v| v.unwrap()), None);

            println!("\x1b[1;33mASYNC STREAM\x1b[0m:");
            while let Some(char) = stream.next().await {
                print!("{}", char);
            }
            println!();
        })
    }
}

fn tokenizer() {
    { 
        use std::{fs::File, io::{BufReader, Read}};
        use wml::{Tokenizer, TokenizerOptions};

        let reader = BufReader::new(File::open("examples/sample.css").unwrap());
        let stream = Tokenizer::new(reader.bytes().map(|v| v.unwrap()), TokenizerOptions::default());

        println!("\x1b[1;33mSYNC TOKENIZER\x1b[0m:");
        for token in stream {
            println!("{:?}", token);
        }

        let reader = BufReader::new(File::open("examples/sample.css").unwrap());
        let stream = Tokenizer::new(reader.bytes().map(|v| v.unwrap()), TokenizerOptions::default());
        println!("{}", stream.collect::<String>());
    }

    // ASYNC
    #[cfg(feature="async")]
    { 
        use smol::{io::{BufReader, AsyncReadExt}, fs::File};
        use wml::{AsyncTokenizer, TokenizerOptions};

        smol::block_on(async move {
            let reader = BufReader::new(File::open("examples/sample.css").await.unwrap());
            let mut stream = AsyncTokenizer::new(reader.bytes().map(|v| v.unwrap()), TokenizerOptions::default());

            println!("\x1b[1;33mASYNC TOKENIZER\x1b[0m:");
            while let Some(token) = stream.next().await {
                println!("{:?}", token);
            }

            let reader = BufReader::new(File::open("examples/sample.css").await.unwrap());
            let mut stream = AsyncTokenizer::new(reader.bytes().map(|v| v.unwrap()), TokenizerOptions::default());
            while let Some(token) = stream.next().await {
                print!("{}", token);
            }
            println!()
        });
    }
}

fn main() {
    code_point_stream();
    tokenizer();
}
