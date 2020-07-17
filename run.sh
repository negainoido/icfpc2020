#!/bin/sh

# https://github.com/icfpcontest2020/starterkit-rust/blob/4afd3ffceacb3000fe7321c40deb5dd3aa746a5b/run.sh

./target/debug/cympfh "$@" || echo "run error code: $?"
