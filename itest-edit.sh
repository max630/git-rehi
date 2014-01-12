#!/bin/sh

file="$1"

echo File: >>/dev/stderr
cat "$file" >>/dev/stderr

cat >"$file" <<EOF
pick 76dee8a19ec
EOF
