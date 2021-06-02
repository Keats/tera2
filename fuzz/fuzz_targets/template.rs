#![no_main]
use libfuzzer_sys::fuzz_target;

extern crate parser_test;

use parser_test::Parser;

fuzz_target!(|data: &[u8]| {
    if let Ok(s) = String::from_utf8(data.to_vec()){
        let mut parser = Parser::new(&s);
        let _ = parser.parse();
    }
});