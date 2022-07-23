#![no_main]
use libfuzzer_sys::fuzz_target;

use std::io::{self, Write};

fuzz_target!(|data: &[u8]| {
    let stdout = io::stdout();
    let mut handle = stdout.lock();

    if let Ok(text) = std::str::from_utf8(data) {
        writeln!(handle, "Fuzzing {:?}\n\n", data).unwrap();
        let _ = rnix::Root::parse(text);
    }
});
