use std::hint::black_box;

use criterion::{criterion_group, criterion_main, Criterion, Throughput};
use rowan::ast::AstNode;

fn all_packages(c: &mut Criterion) {
    let input = include_str!("all-packages.nix");
    let mut group = c.benchmark_group("all-packages");
    group.throughput(Throughput::Bytes(input.len() as u64));
    group.sample_size(30);
    group.bench_with_input("all-packages", input, move |b, input| {
        b.iter(|| rnix::Root::parse(input))
    });
    group.finish();
}

fn tokenizer(c: &mut Criterion) {
    let input = include_str!("all-packages.nix");
    let mut group = c.benchmark_group("tokenizer");
    group.throughput(Throughput::Bytes(input.len() as u64));
    group.sample_size(30);
    group.bench_with_input("all-packages", input, |b, input| {
        b.iter(|| rnix::tokenize(black_box(input)).count())
    });
    group.finish();
}

fn normalized_parts(c: &mut Criterion) {
    let input = include_str!("all-packages.nix");
    let parse = rnix::Root::parse(input);
    let strings = parse.syntax().descendants().filter_map(rnix::ast::Str::cast).collect::<Vec<_>>();
    assert!(!strings.is_empty());

    let mut group = c.benchmark_group("normalized-parts");
    group.sample_size(30);
    group.bench_function("all-packages", |b| {
        b.iter(|| {
            for string in &strings {
                black_box(string.normalized_parts());
            }
        })
    });
    group.finish();
}

criterion_group!(benches, all_packages, tokenizer, normalized_parts);
criterion_main!(benches);
