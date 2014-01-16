#!/bin/sh

# TODO: set up some trap to indicate that the error happened
set -e

# git clone ../rebase2-test-repo /tmp/git-rebase/test-repo
DIR=`pwd`
cd /tmp/git-rebase/test-repo

testee() {
    false
}

if [ -n $GITREBASE2_TEST_SYSTEM ]; then
    testee() {
        git rebase2 "$@"
    }
else
    testee() {
        "$DIR/git-rebase2" "$@"
    }
fi

reset_repo() {
    git checkout -f --no-track -B master origin/master
    git clean -f -x -d
}

# SMOKE
git reset --hard origin/b2
testee origin/b1
git diff --quiet origin/master

# SMOKE EDIT
reset_repo
git reset --hard origin/b2
(
    export GIT_SEQUENCE_EDITOR=/bin/true
    testee -i origin/b1
)
git diff --quiet origin/master

# SMOKE EDIT B2 --> B1
reset_repo
git reset --hard origin/b1
(
    export GIT_SEQUENCE_EDITOR="$DIR/itest-edit.sh"
    testee -i HEAD
)
git diff --quiet origin/master

echo ALL PASSED
