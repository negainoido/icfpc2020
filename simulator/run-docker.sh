#!/bin/bash

set -e


if [[ -z $1 ]]; then
  echo "Please set input file"
  exit 1
fi

if [[ -z $2 ]]; then
  echo "Please set solution file"
  exit 1
fi

script_path=$(readlink -f "$0")
input_file=$(readlink -f "$1")
solution_file=$(readlink -f "$2")
inputs_dir=$(dirname "$input_file")
solutions_dir=$(dirname "$solution_file")

cd $(dirname "$script_path")

IMAGE_ID=$(docker build -q . --build-arg input="$(basename \"$input_file\")" --build-arg solution="$(basename \"$solution_file\")")
docker run -v /dev/shm:/dev/shm -v "$inputs_dir":/app/inputs -v "$solutions_dir":/app/solutions $IMAGE_ID