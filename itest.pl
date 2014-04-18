#!/usr/bin/env perl

use strict;

use Carp;
use Cwd;
use Data::Dumper;
use Test::More;

# {{{ initialize
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
    my $goal_status = do { if (scalar @_) { $_[1]; } else { 0; }; };
    my $output = `$cmd 2>&1`;
    my $status = $?;

    $cmd_num++;
    if ($status == $goal_status) {
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
        bless $res, $class;
        $ENV{$name} = $new_value;
        return $res;
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
