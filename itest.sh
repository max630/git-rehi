#!/bin/sh

# TODO: set up some trap to indicate that the error happened
set -e

# git clone ../rebase2-test-repo /tmp/git-rebase/test-repo
DIR=`pwd`
cd /tmp/git-rebase/test-repo

testee() {
    false
}

if [ -n "$GITREBASE2_TEST_SYSTEM" ]; then
    testee() {
        git rebase2 "$@"
    }
else
    testee() {
        "$DIR/git-rebase2" "$@"
    }
fi

reset_repo() {
    git rebase2 --abort || true
    git checkout -f --no-track -B master origin/master1
    git clean -f -x -d
}

fail() {
    echo "FAIL: $@"
    false
}

test_loud() {
    test "$@" || fail "test $@"
}

# SMOKE
reset_repo
git reset --hard origin/b2
testee origin/b1
git diff --quiet origin/master1

# SMOKE EDIT
reset_repo
git reset --hard origin/b2
(
    export GIT_SEQUENCE_EDITOR=/bin/true
    testee -i origin/b1
)
git diff --quiet origin/master1

# SMOKE EDIT B2 --> B1
reset_repo
git reset --hard origin/b1
(
    export GIT_SEQUENCE_EDITOR="$DIR/itest-edit.sh"
    testee -i HEAD
)
git diff --quiet origin/master1

# rebase with explicit target
reset_repo
git reset --hard origin/b1
git branch -f tmp origin/b2
(
    testee HEAD tmp
    head=`git symbolic-ref HEAD`
    test_loud "$head" = refs/heads/tmp
    git diff --quiet origin/master1 || fail "git diff --quiet origin/master1"
)

# gh-17 Abort checks out old dest_to, should checkout out older HEAD if they are different
reset_repo
git reset --hard origin/b1
git branch -f tmp origin/b2
(
    export GIT_SEQUENCE_EDITOR="$DIR/itest-edit.sh"
    export GIT_SEQUENCE_EDITOR_CASE="fail"
    old_master=`git show --quiet --pretty=format:%h master`
    ! testee -i HEAD tmp
    testee --abort
    head=`git symbolic-ref HEAD`
    new_master=`git show --quiet --pretty=format:%h master`
    test_loud "$head" = refs/heads/master
    test_loud "$old_master" = "$new_master"
)

(
    reset_repo
    git reset --hard origin/master1
    testee origin/base ..origin/b1..
    git diff --quiet origin/master1 || fail "git diff --quiet origin/master1"
)

(
    export GIT_SEQUENCE_EDITOR="$DIR/itest-edit.sh"
    export GIT_SEQUENCE_EDITOR_CASE="merge-c"
    reset_repo
    git reset --hard origin/master1
    testee -i origin/b2
    git diff --quiet origin/master1 || fail "git diff --quiet origin/master1"
    body=`git log --pretty=format:%B -1`
    test "$body" = merge || fail "commit message"
)

(
    # make sure fastforward merge fails
    export GIT_SEQUENCE_EDITOR="$DIR/itest-edit.sh"
    export GIT_SEQUENCE_EDITOR_CASE="merge-c"
    reset_repo
    git reset --hard origin/master1
    ! testee -i origin/b1 || fail "Fastforward should fail!"
)

(
    reset_repo
    git reset --hard origin/b2b3
    ! testee origin/base ..origin/b3.. || fail "Should conflict"
    ! testee --continue || fail "Should not allow continue without resolving"
    git checkout origin/b2b3 -- file1
    git add file1
    export GIT_EDITOR="$DIR/itest-edit.sh"
    export GIT_SEQUENCE_EDITOR_CASE="merge-resolved"
    testee --continue
    git diff --quiet origin/b2b3 || fail "git diff --quiet origin/b2b3"
    body=`git log --pretty=format:%B -1`
    test "$body" = "Merge origin/b2 with resolving conflict (test)" || fail "incorrect commit message"
)

(
    reset_repo
    git reset --hard origin/b2b3
    ! testee origin/base ..origin/b3..
    test -f ".git/rebase2/todo.backup"
)

(
    reset_repo
    git reset --hard origin/master1
    export GIT_EDITOR="$DIR/itest-edit.sh"
    export GIT_SEQUENCE_EDITOR_CASE="merge-inner"
    testee -i origin/base~1 ..origin/base
    git diff --quiet origin/master1 || fail "git diff --quiet origin/master1"
    has_master1=`git branch -r --merged=HEAD origin/master1`
    has_b1=`git branch -r --merged=HEAD origin/b1`
    has_b2=`git branch -r --merged=HEAD origin/b2`
    test "$has_master1$has_b1$has_b2" = "" || fail "source branches should not be merged"
)

echo ALL PASSED
