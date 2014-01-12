#!/bin/sh

# TODO: set up some trap to indicate that the error happened
set -e

# git clone ../rebase2-test-repo /tmp/git-rebase/test-repo
DIR=`pwd`
cd /tmp/git-rebase/test-repo

reset_repo() {
    git checkout -f --no-track -B master origin/master
    git clean -f -x -d
}

# SMOKE
git reset --hard origin/b2
"$DIR/git-rebase2" origin/b1
git diff --quiet origin/master

# SMOKE EDIT
reset_repo
git reset --hard origin/b2
env GIT_SEQUENCE_EDITOR=/bin/true "$DIR/git-rebase2" -i origin/b1
git diff --quiet origin/master

echo ALL PASSED
