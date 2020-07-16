#!/bin/bash

set -e

if [[ $# -ne 3 ]]; then
    echo "
Usage:

$ $0 <image_id> <input file> <solution file>

e.g.

$ $0 simulator:latest ./inputs/input.txt ./solutions/solution.txt

" 1>&2
    exit 1
fi

script_path=$(readlink -f "$0")
input_file=$(readlink -f "$2")
solution_file=$(readlink -f "$3")
inputs_dir=$(dirname "$input_file")
solutions_dir=$(dirname "$solution_file")

cd $(dirname "$script_path")
cp "$input_file" ./tmp/input.txt
cp "$solution_file" ./tmp/solution.txt

docker run -v /dev/shm:/dev/shm -v "$(pwd)/tmp":/app/files "$1"