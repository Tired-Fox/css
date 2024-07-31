use futures_util::{StreamExt};



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
        //use smol::{io::{BufReader, AsyncReadExt}, fs::File};

        use tokio_util::codec::{FramedRead, BytesCodec};
        use tokio::fs::File;

        use wml::AsyncCodePointStream;

        tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build().unwrap()
            .block_on(async move {
                let reader = FramedRead::new(File::open("examples/sample.css").await.unwrap(), BytesCodec::new());
                let data = reader.flat_map(|v| futures_util::stream::iter(v.unwrap().freeze()));
                //println!("{}", data.map(|v| v as char).collect::<String>().await);
                let mut stream = AsyncCodePointStream::new(data, None);
                println!("\x1b[1;33mASYNC\x1b[0m:");
                while let Some(char) = stream.next().await {
                    print!("{}", char);
                }
                println!();
            })
            
    }
}
