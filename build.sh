#!/bin/sh

# https://github.com/icfpcontest2020/starterkit-rust/blob/4afd3ffceacb3000fe7321c40deb5dd3aa746a5b/build.sh

# use --release later if necessary
cargo build --offline --bin submission
