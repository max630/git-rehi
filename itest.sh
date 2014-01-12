#!/bin/sh

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
