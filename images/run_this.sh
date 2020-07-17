#!/bin/bash

cd $(dirname "$0")

for i in {1..42}
do
    wget -q "https://message-from-space.readthedocs.io/en/latest/_images/message${i}.png" \
         -O "message${i}.png"
done
