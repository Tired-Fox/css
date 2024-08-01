use futures_util::StreamExt;

fn main() {
    // SYNC
    {
        use std::{fs::File, io::{BufReader, Read}};
        use wml::CodePointStream;

        let reader = BufReader::new(File::open("examples/sample.css").unwrap());
        let mut stream = CodePointStream::new(reader.bytes().map(|v| v.unwrap()), None);

        println!("\x1b[1;33mSYNC\x1b[0m:");
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

            println!("\x1b[1;33mASYNC\x1b[0m:");
            while let Some(char) = stream.next().await {
                print!("{}", char);
            }
            println!();
        })
    }
}
