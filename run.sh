#!/bin/sh

/solution/target/debug/app "$@" || echo "run error code: $?"
