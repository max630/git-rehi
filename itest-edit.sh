#!/bin/sh

file="$1"

echo File: >>/dev/stderr
cat "$file" >>/dev/stderr


case "$GIT_SEQUENCE_EDITOR_CASE" in
fail)
cat >"$file" <<EOF
x false
EOF
;;
*)
cat >"$file" <<EOF
pick 76dee8a19ec
EOF
;;
esac
