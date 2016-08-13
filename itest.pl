#!/usr/bin/env perl

use strict;

use Carp;
use Cwd;
use Data::Dumper;
use Test::More qw(no_plan);

# {{{ initialize
if (! -e "/tmp/git-rebase/test-repo") {
    !system("mkdir -p /tmp/git-rebase/test-repo && ( cd /tmp/git-rebase/test-repo && git init && git fast-import ) <itest-repo.data")
        or die("Test repo initialization failed: $? ($!)");
}
my $SOURCE_DIR = getcwd;
chdir("/tmp/git-rebase/test-repo") or die("Unable to chdir to /tmp/git-rebase/test-repo: $!");
my $testee = "$SOURCE_DIR/git-rehi";
# my $testee = "git rehi";
# }}}

# {{{ test utils
my @Tests = ();
sub t(&*) { my ($block, $name) = @_;
    push @Tests, [$name, $block];
}

my $cmd_num = 0;
sub cmd($;$) { my ($cmd) = @_;
    my $goal_status = do { if (scalar @_ > 1) { $_[1]; } else { 0; }; };
    my $output = `$cmd 2>&1 </dev/null`;
    my $status = $?;

    $cmd_num++;
    if ($goal_status eq "!= 0" && $status != 0
        || $goal_status =~ /^-?\d+$/ && $status == $goal_status)
    {
        if ($output =~ /at.*line [0-9]+/) {
            diag("Command output contains stacktrace\nCommand: $cmd\nOutput:\n$output\n");
            fail("cmd$cmd_num");
        } else {
            ok("cmd$cmd_num");
        }
    } else {
        diag("Command status does not match: $status vs $goal_status\nCommand: $cmd\nOutput:\n$output\n");
        fail("cmd$cmd_num");
    }
}

sub reset_repo {
    `$testee --abort 2>&1`;
    `git checkout -f --no-track -B master origin/master1 2>&1`;
    die("reset checkout failed") if ($? != 0);
    `git clean -f -x -d 2>&1`;
    die("reset clean failed") if ($? != 0);
}

{
    package env_guard;
    sub new ($$) { my ($class, $name, $new_value) = @_;
        my $res = { name => $name, prev_exists => exists $ENV{$name}, prev_value => $ENV{$name} };
        $ENV{$name} = $new_value;
        return (bless $res, $class);
    }

    sub DESTROY($) { my ($instance) = @_;
        my $name = $instance->{name};
        if ($instance->{prev_exists}) {
            #print "return $name <-- $instance->{prev_value}";
            $ENV{$name} = $instance->{prev_value};
        } else {
            #print "remove $name";
            delete $ENV{$name};
        }
    }
}
# }}}

t {
    cmd("git reset --hard origin/b2");
    cmd("$testee origin/b1");
    cmd("git diff --quiet origin/master1");
} smoke;

t {
    my $g = env_guard->new("GIT_SEQUENCE_EDITOR", "/bin/true");
    cmd("git reset --hard origin/b2");
    cmd("$testee -i origin/b1");
    cmd("git diff --quiet origin/master1");
} smoke_edit;

t {
    cmd("git reset --hard origin/b1");
    my $g = env_guard->new("GIT_SEQUENCE_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    cmd("$testee -i HEAD");
    cmd("git diff --quiet origin/master1");
} edit_b2_to_b1;

t {
    my $g = env_guard->new("GIT_SEQUENCE_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "empty");
    cmd("$testee -i origin/b4 ..origin/base");
    cmd("git diff --quiet origin/master1");
} edit_empty;

t {
    my $g = env_guard->new("GIT_SEQUENCE_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "empty-with-comment");
    cmd("$testee -i origin/b4 ..origin/base");
    cmd("git diff --quiet origin/master1");
} edit_empty_with_comment;

t {
    cmd("git reset --hard origin/b1");
    cmd("git branch -f tmp origin/b2");
    cmd("$testee HEAD tmp");
    is(`git symbolic-ref HEAD`, "refs/heads/tmp\n");
    cmd("git diff --quiet origin/master1");
} explicit_target;

t {
    cmd("git reset --hard origin/b1");
    cmd("git branch -f tmp origin/b2");
    my $g = env_guard->new("GIT_SEQUENCE_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "fail");
    my $old_master = `git show --quiet --pretty=format:%h master`;
    cmd("$testee -i HEAD tmp", "!= 0");
    cmd("$testee --abort");
    is(`git symbolic-ref HEAD`, "refs/heads/master\n");
    is(`git show --quiet --pretty=format:%h master`, $old_master);
} gh_17;

t {
    cmd("$testee origin/base ..origin/b1..");
    cmd("git diff --quiet origin/master1");
} parsing;

t {
    my $g = env_guard->new("GIT_SEQUENCE_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "merge-c");
    cmd("$testee -i origin/b2");
    cmd("git diff --quiet origin/master1");
    is(`git log --pretty=format:%B -1`, "merge\n");
} edit_merge;

t {
    my $g = env_guard->new("GIT_SEQUENCE_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "merge-no-ff");
    cmd("$testee -i origin/b4");
    is_deeply([split(/[ \n]/,`git show --quiet --pretty=format:%p HEAD`)],
              [split(/\n/, `git show --quiet --pretty=format:%h origin/b4 origin/b2`)]);
} edit_merge_no_ff;

t {
    my $g = env_guard->new("GIT_SEQUENCE_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "merge-no-ff-reuse");
    cmd("git reset --hard origin/b2");
    cmd("$testee -i origin/b4");
    is_deeply([split(/[ \n]/,`git show --quiet --pretty=format:%p HEAD`)],
              [split(/\n/, `git show --quiet --pretty=format:%h origin/b4 origin/b2`)]);
} edit_merge_no_ff_reuse;

t {
    my $g = env_guard->new("GIT_SEQUENCE_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "merge-no-c");
    cmd("$testee -i origin/b2");
    cmd("git diff --quiet origin/master1");
} edit_merge_no_c;

t {
    my $g = env_guard->new("GIT_SEQUENCE_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "merge-c");
    cmd("$testee -i origin/b1", "!= 0");
} fastforward_merge_fails;

t {
    cmd("git reset --hard origin/b4");
    cmd("git commit --allow-empty -m UPDATE");
    cmd("$testee HEAD origin/b4..origin/b3..origin/b2b3", "!= 0"); # conflict
    cmd("$testee --continue", "!= 0"); # no continue without resolving
    cmd("git checkout origin/b2b3 -- file1");
    cmd("git add file1");
    my $ge = env_guard->new("GIT_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "merge-resolved");
    cmd("$testee --continue");
    cmd("git diff --quiet origin/b2b3");
    is(`git log --pretty=format:%B -1`, "Merge origin/b2 with resolving conflict (test)\n");
} merge_conflict;

t {
    cmd("$testee origin/b1");
    like(`cat .git/rehi_todo.backup`, qr/^merge -c [0-9a-z]+ [0-9a-z]+,HEAD merge$/);
    is(`git show --quiet --pretty=format:%h HEAD`,
       `git show --quiet --pretty=format:%h origin/master1`);
} merge_second_parent;

t {
    cmd("git reset --hard origin/base");
    cmd("git commit --allow-empty -m UPDATE");
    cmd("$testee HEAD origin/base..origin/b3..origin/b2b3", "!= 0");
    ok(-f ".git/rehi/todo.backup");
} todo_backup;

t {
    my $g = env_guard->new("GIT_SEQUENCE_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "merge-inner");
    cmd("$testee -i origin/base~1 ..origin/base");
    cmd("git diff --quiet origin/master1");
    is(`git branch -r --merged=HEAD origin/master1` . `git branch -r --merged=HEAD origin/b1` . `git branch -r --merged=HEAD origin/b2`,
       "",
       "source branches are not merged");
} inner_merge_handmade;

t {
    cmd("git reset --hard origin/b4");
    cmd("git commit --allow-empty -m 'LOCAL'");
    cmd("$testee HEAD ..origin/master1");
    cmd("git diff --quiet origin/master1");
    is(`git branch -r --merged=HEAD origin/master1` . `git branch -r --merged=HEAD origin/b1` . `git branch -r --merged=HEAD origin/b2`,
       "",
       "source branches are not merged");
} inner_merge_detected;

t {
    cmd("git reset --hard origin/b2b3");
    cmd("$testee origin/b4");
    is(`git show --quiet --pretty=format:%h HEAD`,
       `git show --quiet --pretty=format:%h origin/b2b3`);
} fastforward_over_merges;

t {
    my $g = env_guard->new("GIT_SEQUENCE_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "merge-inner");
    cmd("$testee -i origin/b4");
    {
        my $gc2 = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "merge-inner-broken");
        if (-f "save_todo") { cmd("rm save_todo"); }
        cmd("$testee -i origin/b4", "!= 0"); # unknown refs - should fail
        ok(-f "save_todo"); # make sure it reached editor
    }
} marks_cleared;

t {
    cmd("git reset --hard origin/b_noffmerge");
    cmd("$testee origin/b4 ..origin/base..");
    is(`git show --quiet --pretty=format:%h HEAD`,
       `git show --quiet --pretty=format:%h origin/b_noffmerge`);
} handle_noff_merges;

t {
    cmd("git reset --hard origin/b_diamond_after_merge");
    cmd("$testee origin/b2");
} inner_merge_after_merge;

t {
    cmd("git reset --hard origin/b_merge_of_merges");
    cmd("$testee origin/b5");
} base_after_merge;

t {
    my $g = env_guard->new("GIT_SEQUENCE_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc2 = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "fail");
    cmd("$testee -i origin/base", "!= 0");
    cmd("grep -q 'pick.*change2\$' save_todo");
} optimal_first_parent;

t {
    my $g = env_guard->new("GIT_SEQUENCE_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc2 = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "fail");
    cmd("$testee -i origin/base origin/b1~1..", "!= 0");
    cmd("grep -q 'pick.*change1\$' save_todo");
} optimal_include_start_from_sourcefrom;

t {
    my $g = env_guard->new("GIT_SEQUENCE_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc2 = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "fail");
    cmd("$testee -i origin/b1~1", "!= 0");
    cmd("grep -q 'pick.*change1\$' save_todo");
} optimal_include_start_from_base;

t {
    my $g = env_guard->new("GIT_SEQUENCE_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc2 = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "fail");
    cmd("$testee -i HEAD", "!= 0");
    is(`$testee --current`, "Current: exec false\n");
} current_ok;

t {
    cmd("$testee --current", "!= 0");
} current_no_rebase;

t {
    my $g = env_guard->new("GIT_SEQUENCE_EDITOR", "/bin/true");
    cmd("git reset --hard origin/b3");
    cmd("git merge -sours -m ours_with_ref_comment origin/b2");
    cmd("git branch -f t_b3b2_ours");
    cmd("git reset --hard origin/b3");
    cmd("git commit --allow-empty -m dummy");
    cmd("$testee -i HEAD t_b3b2_ours~1..t_b3b2_ours");
    is("ours_with_ref_comment\n", `git log --pretty=format:%B -1`);
} ours_with_ref;

my %argv_idx = ();
if (scalar @ARGV) {
    @argv_idx{@ARGV} = ();
}
foreach my $test (@Tests) {
    if (!scalar %argv_idx || exists $argv_idx{$test->[0]}) {
        reset_repo;
        if($] >= "5.012000") {
            subtest $test->[0] => $test->[1];
        } else {
            print "test: " . $test->[0] . "\n";
            $test->[1]();
            print "end: " . $test->[0] . "\n";
        }
    }
}

# vim: foldmethod=marker
