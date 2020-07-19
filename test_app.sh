#!/bin/bash


resp=$(cargo run --bin app -- https://icfpc2020-api.testkontur.ru | tail -n 1)

echo $resp

attacker=$(echo "${resp}" | awk '{print $1}')

defender=$(echo "${resp}" | awk '{print $2}')

echo $attacker $defender


cargo run --bin app -- https://icfpc2020-api.testkontur.ru $attacker &
cargo run --bin app -- https://icfpc2020-api.testkontur.ru $defender &

trap "kill 0" EXIT

wait
