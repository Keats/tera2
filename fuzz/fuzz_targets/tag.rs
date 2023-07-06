#![no_main]
use libfuzzer_sys::fuzz_target;

extern crate tera;

use tera::Parser;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = String::from_utf8(data.to_vec()){
        let val = format!("{{% {} %}}", s);
        let parser = Parser::new(&val);
        let _ = parser.parse();
    }
});
