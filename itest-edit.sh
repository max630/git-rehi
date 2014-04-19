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
merge-c)
cat >"$file" <<EOF
merge -c 3a5191091 HEAD,608a449bb2 Test merge subject
EOF
;;
merge-resolved)
cat >"$file" <<EOF
Merge origin/b2 with resolving conflict (test)
EOF
;;
merge-inner)
cat >"$file" <<EOF
x git commit --allow-empty -m UPDATE
pick origin/base
: tmp1
pick origin/b1
: tmp2
reset @tmp1
pick origin/b2
merge -c origin/master1 HEAD,@tmp2
EOF
;;
merge-inner-broken)
cat >"$file" <<EOF
reset @tmp1
pick origin/b2
merge -c origin/master1 HEAD,@tmp2
EOF
;;
*)
cat >"$file" <<EOF
pick 76dee8a19ec
EOF
;;
esac
