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
merge-c-nocomment)
cat >"$file" <<EOF
merge -c 3a5191091 HEAD 608a449bb2
EOF
;;
*)
cat >"$file" <<EOF
pick 76dee8a19ec
EOF
;;
esac
