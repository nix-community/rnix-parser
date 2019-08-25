#!/bin/sh

for nix in $(find test_data/ -name "*.nix"); do
    expected="$(echo -n "$nix" | sed "s/\.nix$//").expect"
    code="$(cat "$nix")"
    code="${code%\n}"
    echo -n "$code" | cargo run --example dump-ast "/dev/stdin" > "$expected" 2>/dev/null
done
