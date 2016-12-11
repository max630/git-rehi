#!/usr/bin/env perl

use strict;

use Carp;
use Cwd;
use Data::Dumper;
use Test::More qw(no_plan);

# {{{ initialize
if (! -e "/tmp/git-rehi/test-repo") {
    !system("mkdir -p /tmp/git-rehi/test-repo && ( cd /tmp/git-rehi/test-repo && git init && git fast-import && git config user.email test.author\@example.com && git config user.name TestAuthor ) <itest-repo.data")
        or die("Test repo initialization failed: $? ($!)");
}
my $SOURCE_DIR = getcwd;
my $STACK_ROOT = `stack path --local-install-root`;
$STACK_ROOT =~ s/[\n\r]+$//;
$STACK_ROOT =~ s/\\/\//g;
chdir("/tmp/git-rehi/test-repo") or die("Unable to chdir to /tmp/git-rehi/test-repo: $!");
my $testee = $STACK_ROOT . "/bin/git-rehi";
# }}}

# {{{ test utils
my @Tests = ();
sub t(&*) { my ($block, $name) = @_;
    push @Tests, [$name, $block];
}

my $cmd_num = 0;
sub cmd($;$$) { my ($cmd) = @_;
    my $goal_status = do { if (scalar @_ > 1) { $_[1]; } else { 0; }; };
    my $output_dest = do { if (scalar @_ > 2) { $_[2]; } else { undef; }; };
    my $output = `$cmd 2>&1 </dev/null`;
    my $status = $?;

    $cmd_num++;
    if ($goal_status eq "!= 0" && $status != 0
        || $goal_status =~ /^-?\d+$/ && $status == $goal_status)
    {
       if ($output =~ /Internal error: |Unexpected happened: |IO error: /) {
            diag("Command failed with unexpected error\nCommand: $cmd\nOutput:\n$output\n");
            fail("cmd$cmd_num");
        } else {
            ok("cmd$cmd_num");
        }
    } else {
        diag("Command status does not match: $status vs $goal_status\nCommand: $cmd\nOutput:\n$output\n");
        fail("cmd$cmd_num");
    }
    if (defined $output_dest) {
        ${$output_dest} = $output;
    }
}

my $SUBTEST_COMMIT_COUNT;

sub reset_repo {
    `$testee --abort 2>&1`;
    `git checkout -f --no-track -B master origin/master1 2>&1`;
    die("reset checkout failed") if ($? != 0);
    `git clean -f -x -d 2>&1`;
    die("reset clean failed") if ($? != 0);
    $SUBTEST_COMMIT_COUNT = 0;
}

my $TEST_NAME;
sub tc($;$) { my ($files, $tag) = @_;
    foreach my $filename (keys %{$files}) {
        if (defined $files->{$filename}) {
            write_file($filename, $files->{$filename});
            cmd("git add -- $filename");
        } else {
            cmd("git rm -- $filename");
        }
    }
    my $msg = "$TEST_NAME$SUBTEST_COMMIT_COUNT";
    $SUBTEST_COMMIT_COUNT++;
    cmd("git commit -m $msg -q");
    if (defined $tag) {
        cmd("git tag -f $tag");
    }
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

# {{{ utils
sub write_file($$) { my ($file,$content) = @_;
    open(my $fh, ">", $file);
    print $fh $content;
    close $fh;
}

sub seq($$;$) { my ($from,$to,$prefix) = @_;
    if (!defined $prefix) { $prefix = ""; }
    my $buf = "";
    for (my $i = $from; $i < $to; ++$i) {
        $buf = $buf . $prefix . $i . "\n";
    }
    return $buf;
}
# }}}

t {
    cmd("git reset --hard origin/b2");
    cmd("$testee origin/b1");
    cmd("git diff --quiet origin/master1");
} smoke;

t {
    my $g = env_guard->new("GIT_EDITOR", "/bin/true");
    cmd("git reset --hard origin/b2");
    cmd("$testee -i origin/b1");
    cmd("git diff --quiet origin/master1");
} smoke_edit;

t {
    my $g = env_guard->new("GIT_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "set-comment");
    cmd("git reset --hard origin/b2");
    cmd("$testee -i origin/b1");
    is(`git log --pretty=format:%B -1`, "test-comment\n");
} edit_comment;

t {
    my $g = env_guard->new("GIT_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "edit-noop");
    cmd("git reset --hard origin/b2");
    cmd("$testee -i origin/base");
    cmd("$testee --continue");
    is(`git rev-parse origin/b2`, `git rev-parse HEAD`);
} edit_edit_noop;

t {
    my $g = env_guard->new("GIT_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "edit-noop");
    cmd("git reset --hard origin/b2");
    cmd("$testee -i origin/base");
    is(`git symbolic-ref --quiet HEAD`, "");
} is_detached;

t {
    cmd("git reset --hard origin/b1");
    my $g = env_guard->new("GIT_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    cmd("$testee -i HEAD");
    cmd("git diff --quiet origin/master1");
} edit_b2_to_b1;

t {
    my $g = env_guard->new("GIT_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "empty");
    cmd("$testee -i origin/b4 ..origin/base");
    cmd("git diff --quiet origin/master1");
} edit_empty;

t {
    my $g = env_guard->new("GIT_EDITOR", "$SOURCE_DIR/itest-edit.sh");
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
    my $g = env_guard->new("GIT_EDITOR", "$SOURCE_DIR/itest-edit.sh");
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
    my $g = env_guard->new("GIT_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "merge-c");
    cmd("$testee -i origin/b2");
    cmd("git diff --quiet origin/master1");
    is(`git log --pretty=format:%B -1`, "merge\n");
} edit_merge;

t {
    my $g = env_guard->new("GIT_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "merge-no-ff");
    cmd("$testee -i origin/b4");
    is_deeply([split(/[ \n]/,`git show --quiet --pretty=format:%p HEAD`)],
              [split(/\n/, `git show --quiet --pretty=format:%h origin/b4 origin/b2`)]);
} edit_merge_no_ff;

t {
    my $g = env_guard->new("GIT_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "merge-no-ff-reuse");
    cmd("git reset --hard origin/b2");
    cmd("$testee -i origin/b4");
    is_deeply([split(/[ \n]/,`git show --quiet --pretty=format:%p HEAD`)],
              [split(/\n/, `git show --quiet --pretty=format:%h origin/b4 origin/b2`)]);
} edit_merge_no_ff_reuse;

t {
    my $g = env_guard->new("GIT_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "merge-no-c");
    cmd("$testee -i origin/b2");
    cmd("git diff --quiet origin/master1");
} edit_merge_no_c;

t {
    my $g = env_guard->new("GIT_EDITOR", "$SOURCE_DIR/itest-edit.sh");
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
    my $g = env_guard->new("GIT_EDITOR", "$SOURCE_DIR/itest-edit.sh");
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
    my $g = env_guard->new("GIT_EDITOR", "$SOURCE_DIR/itest-edit.sh");
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
    cmd("git reset --hard origin/base");
    write_file("file", seq(1, 100, "line"));
    cmd("git reset --hard origin/base");
    cmd("git add file");
    cmd("git commit -m base file");
    cmd("git tag -f base");
    cmd("git commit -m dummy --allow-empty");
    cmd("git tag -f rehiBase");
    write_file("file", seq(1, 33, "line") . "line33edit\n" . seq(34,100,"line"));
    cmd("git commit -m b2 file");
    cmd("git tag -f b2");
    cmd("git reset --hard base");
    write_file("file", seq(1, 73, "line") . "line73edit\n" . seq(74,100,"line"));
    cmd("git commit -m b1 file");
    cmd("git merge b2");
    cmd("git tag -f b1");
    cmd("$testee base rehiBase..");
    cmd("git diff --exit-code b1..HEAD");
} merge_into_twice;

t {
    my $g = env_guard->new("GIT_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc2 = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "fail");
    cmd("$testee -i origin/base", "!= 0");
    cmd("grep -q 'pick.*change2\$' save_todo");
} optimal_first_parent;

t {
    my $g = env_guard->new("GIT_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc2 = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "fail");
    cmd("$testee -i origin/base origin/b1~1..", "!= 0");
    cmd("grep -q 'pick.*change1\$' save_todo");
} optimal_include_start_from_sourcefrom;

t {
    my $g = env_guard->new("GIT_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc2 = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "fail");
    cmd("$testee -i origin/b1~1", "!= 0");
    cmd("grep -q 'pick.*change1\$' save_todo");
} optimal_include_start_from_base;

t {
    my $g = env_guard->new("GIT_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc2 = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "fail");
    cmd("$testee -i HEAD", "!= 0");
    is(`$testee --current`, "Current: exec false\n");
} current_exec;

t {
    my $g = env_guard->new("GIT_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc2 = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "fail-pick");
    my $hash = `git rev-parse --short origin/b2`;
    $hash =~ s/[\n\r]*$//s;
    my $gc3 = env_guard->new("GIT_SEQUENCE_EDITOR_STEP_HASH", $hash);
    cmd("$testee -i origin/b3 ..origin/b2b3", "!= 0");
    is(`$testee --current`, "Current: pick $hash change2\n");
} current_pick;

t {
    cmd("$testee --current", "!= 0");
} current_no_rebase;

t {
    tc({ "f1" => "l1\nl2\nl3\n" }, "base");
    tc({ "f1" => "l1\nl2r1\nl3\n" });
    tc({ "f1" => "l1\nl2r2\nl3\n" }, "src_dest");
    cmd("git reset --hard base");
    tc({ "f1" => "l1\nl2l1\nl3\n" });

    my $g = env_guard->new("GIT_EDITOR", "$SOURCE_DIR/itest-edit.sh");
    my $gc = env_guard->new("GIT_SEQUENCE_EDITOR_CASE", "pass");
    cmd("$testee -i HEAD base..src_dest", "!= 0");
    like(`$testee --current`, qr/pick [0-9a-f]+ current_failed_twice1\n/);
    write_file("f1", "l1\nl2l2\nl3\n");
    cmd("git add f1 && git commit --no-edit");
    cmd("$testee --continue", "!= 0");
    like(`$testee --current`, qr/pick [0-9a-f]+ current_failed_twice2\n/);
} current_failed_twice;

t {
    tc({ "failed_pick_prints_conflicts_filename" => "l1\nl2\nl3\n",
         "failed_pick_prints_noconflict_filename" => "l1\nl2\n" }, "base");
    tc({ "failed_pick_prints_conflicts_filename" => "l1\nl2r\nl3\n",
         "failed_pick_prints_noconflict_filename" => "l1\nl2r\n" }, "src_dest");
    cmd("git reset --hard base");
    tc({ "failed_pick_prints_conflicts_filename" => "l1\nl2l\nl3\n" });

    my $out;
    cmd("$testee HEAD base..src_dest", "!= 0", \$out);
    like($out, qr/\bfailed_pick_prints_conflicts_filename\b/);
    unlike($out, qr/\bfailed_pick_prints_noconflict_filename\b/);
} failed_pick_prints_conflicts;


t {
    my $g = env_guard->new("GIT_EDITOR", "/bin/true");
    cmd("git reset --hard origin/b3");
    cmd("git merge -sours -m ours_with_ref_comment origin/b2");
    cmd("git branch -f t_b3b2_ours");
    cmd("git reset --hard origin/b3");
    cmd("git commit --allow-empty -m dummy");
    cmd("$testee -i HEAD t_b3b2_ours~1..t_b3b2_ours");
    is("ours_with_ref_comment\n", `git log --pretty=format:%B -1`);
} ours_with_ref;

t {
    cmd("$testee origin/b5 origin/b5..", "!= 0");
    cmd("$testee origin/b2");
} startfailclean;

t {
    cmd("git reset --hard origin/b2");
    cmd("git tag -a -f -m origin_base origin_base origin/base");
    cmd("$testee origin/b1 origin_base..");
    cmd("git diff --quiet origin/master1");
} annotated_tag;

my %argv_idx = ();
if (scalar @ARGV) {
    @argv_idx{@ARGV} = ();
}
foreach my $test (@Tests) {
    if (!scalar %argv_idx || exists $argv_idx{$test->[0]}) {
        reset_repo;
        $TEST_NAME = $test->[0];
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
