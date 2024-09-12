#!/bin/sh
# David Smith 2022 david.a.c.v.smith@gmail.com
#
# This file has been adapted from David Smith's SmithForth
# (https://dacvs.neocities.org/SF/)

F="SForth"

compile() {
    cut -d'#' -f1 | xxd -p -r
}

bytes() { # little-endian base sixteen
    wc -c | xargs printf '%08X' | sed 's/\(..\)\(..\)\(..\)\(..\)/\4 \3 \2 \1/'
}

replace() {
    sed "/${1}.*build.sh/ s/^\S\S \S\S \S\S \S\S/${2}/"
}

cat ${F}.dmp | compile >${F}0
cat ${F}0 system.fs input.fs >$F
m=`bytes <${F}0`
n=`bytes <$F`
cat ${F}.dmp | replace p_filesz "$n" | compile >${F}0
cat ${F}0 system.fs input.fs >$F
chmod +x $F
