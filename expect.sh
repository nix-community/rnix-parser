#!/bin/sh

for nix in $(find tests/parser -name "*.nix"); do
    expected="$(echo -n "$nix" | sed "s/\.nix$//").expect"
    code="$(cat "$nix")"
    code="${code%\n}"
    echo -n "$code" | cargo run --example dump-ast "/dev/stdin" > "$expected" 2>/dev/null
done
