use criterion::{Criterion, Benchmark, Throughput, criterion_group, criterion_main};
use rnix::parse;

fn all_packages(c: &mut Criterion) {
    let input = include_str!("all-packages.nix");
    c.bench(
        "all-packages",
        Benchmark::new("all-packages", move |b| b.iter(|| {
                parse(input)
            }))
            .throughput(Throughput::Bytes(input.len() as u32))
            .sample_size(30)
    );
}

criterion_group!(benches, all_packages);
criterion_main!(benches);
