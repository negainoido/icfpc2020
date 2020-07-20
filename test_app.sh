#!/bin/bash


resp=$(cargo run --bin app -- https://icfpc2020-api.testkontur.ru | tail -n 1)

echo $resp

attacker=$(echo "${resp}" | awk '{print $1}')

defender=$(echo "${resp}" | awk '{print $2}')

echo $attacker $defender


cargo run --bin app -- https://icfpc2020-api.testkontur.ru $attacker "1,1,1" | tee attacker.log &
cargo run --bin app -- https://icfpc2020-api.testkontur.ru $defender "1,1,1" | tee defender.log &

trap "kill 0" EXIT

wait
