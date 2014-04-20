#!/usr/bin/env perl

use strict;

use Carp;
use Cwd;
use Data::Dumper;
use Test::More;

# {{{ initialize
if (! -e "/tmp/git-rebase/test-repo") {
    system("git clone git://github.com/max630/git-rebase2-test-repo.git /tmp/git-rebase/test-repo") and die("Initial cloning failed");
}
my $SOURCE_DIR = getcwd;
chdir("/tmp/git-rebase/test-repo") or die("Unable to chdir to /tmp/git-rebase/test-repo: $!");
my $testee = "$SOURCE_DIR/git-rebase2";
# my $testee = "git rebase2";
# }}}

# {{{ test utils
my @Tests = ();
sub t(&*) { my ($block, $name) = @_;
    push @Tests, [$name, $block];
}

my $cmd_num = 0;
sub cmd($;$) { my ($cmd) = @_;
    my $goal_status = do { if (scalar @_ > 1) { $_[1]; } else { 0; }; };
    my $output = `$cmd 2>&1`;
    my $status = $?;

    $cmd_num++;
    if ($goal_status eq "!= 0" && $status != 0
        || $goal_status =~ /^-?\d+$/ && $status == $goal_status)
    {
        ok("cmd$cmd_num");
    } else {
        diag("Command status does not match: $status vs $goal_status\nCommand: $cmd\nOutput:\n$output\n");
        fail("cmd$cmd_num");
    }
}

sub reset_repo {
    `git rebase2 --abort 2>&1`;
    `git checkout -f --no-track -B master origin/master1 2>&1`;
    die("reset checkout failed") if ($? != 0);
    `git clean -f -x -d 2>&1`;
    die("reset clean failed") if ($? != 0);
}

package env_guard {
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
    my $gc = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "merge-c");
    cmd("$testee -i origin/b1", "!= 0");
} fastforward_merge_fails;

t {
    cmd("git reset --hard origin/b2b3");
    cmd("$testee origin/base ..origin/b3..", "!= 0"); # conflict
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
    cmd("git reset --hard origin/b2b3");
    cmd("$testee origin/base ..origin/b3..", "!= 0");
    ok(-f ".git/rebase2/todo.backup");
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
    my $g = env_guard->new("GIT_SEQUENCE_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "merge-inner");
    cmd("$testee -i origin/base~1");
    {
        my $gc2 = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "merge-inner-broken");
        cmd("$testee -i origin/base~1", "!= 0"); # unknown refs - should fail
    }
} marks_cleared;

my %argv_idx = ();
if (scalar @ARGV) {
    @argv_idx{@ARGV} = ();
}
foreach my $test (@Tests) {
    if (!scalar %argv_idx || exists $argv_idx{$test->[0]}) {
        reset_repo;
        subtest $test->[0] => $test->[1];
    }
}

done_testing();

# vim: foldmethod=marker
