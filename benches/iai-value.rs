use std::hint::black_box;
use std::path::PathBuf;

use iai_callgrind::library_benchmark;
use iai_callgrind::library_benchmark_group;
use iai_callgrind::main;
use serde::Serialize;

use tera::value::Value;

#[derive(Serialize, Default)]
struct Page {
    path: PathBuf,
    title: String,
    summary: String,
    content: String,
    permalink: String,
    draft: bool,
    generate_feed: bool,
    word_count: usize,
    reading_time: usize,
    backlinks: Vec<String>,
    pages: Vec<Page>,
}

#[library_benchmark]
#[bench::page(&Page::default())]
fn serialize_value<T>(value: &T)
where
    T: Serialize + ?Sized,
{
    black_box(Value::from_serializable(value));
}

library_benchmark_group!(name = serialize; benchmarks = serialize_value);
main!(library_benchmark_groups = serialize);
