use std::path::PathBuf;

use criterion::{Criterion, black_box, criterion_group, criterion_main};
use serde_derive::Serialize;

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

fn criterion_benchmark(c: &mut Criterion) {
    let page = Page::default();

    c.bench_function("serialization", |b| {
        b.iter(|| {
            black_box(Value::from_serializable(&page));
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
