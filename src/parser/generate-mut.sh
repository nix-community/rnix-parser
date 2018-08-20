#!/bin/sh

sed -e '/^.*\/\/\s*IMMUT\s*$/d' -e '/^\s*\/\/\s*<<IMMUT$/,/^\s*\/\/\s*IMMUT>>$/d' -e 's/^\(\s*\)\/\/\s*MUT\s\?/\1/' children.rs > children_mut.rs
