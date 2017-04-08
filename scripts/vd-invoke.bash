#!/usr/bin/env bash

set -eu

# * 10,14 * * 1,2,3,4,5 cd ~/zn; ./scripts/vd-invoke.bash

chan="#developerslv"

grep -qE "^$(date +%F)[^	]+	[^	]+	!vd" "./data/logs/$chan.log" || \
    ./scripts/raw-socket-send.rb PRIVMSG "$chan" '!vd'
