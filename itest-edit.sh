#!/bin/sh

file="$1"

echo File: >>/dev/stderr
cat "$file" >>/dev/stderr
cp "$file" "save_todo"

case "$GIT_SEQUENCE_EDITOR_CASE" in
empty)
: >"$file"
;;
empty-with-comment)
cat >"$file" <<EOF
#bla-bla
EOF
;;
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
merge-no-c)
cat >"$file" <<EOF
merge HEAD,608a449bb2 Some subject
EOF
;;
merge-no-ff)
cat >"$file" <<EOF
: base
pick origin/base
pick origin/b2
: tmp1
reset @base
merge --no-ff HEAD,@tmp1
EOF
;;
merge-no-ff-reuse)
file_content=`awk -- '/^end$/{ exit } { print }' "$file"`
cat >"$file" <<EOF
: base
$file_content
: tmp1
#aaaa
reset @base
merge --no-ff HEAD,@tmp1
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
#bbbb
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
