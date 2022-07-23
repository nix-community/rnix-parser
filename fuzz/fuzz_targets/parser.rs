#![no_main]
use libfuzzer_sys::fuzz_target;

use std::io::{self, Write};

fuzz_target!(|data: &[u8]| {
    if let Ok(text) = std::str::from_utf8(data) {
        let _ = rnix::Root::parse(text);
    }
});

