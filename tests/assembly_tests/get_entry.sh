#! /usr/bin/env bash
entry=$(mips-img-linux-gnu-readelf --program-headers $1 | grep "Entry point")

readarray -d " " strarr <<< "$entry"
echo "${strarr[2]}"
