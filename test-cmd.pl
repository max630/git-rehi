#!/usr/bin/env perl

use strict;

use Data::Dumper;
use B::Deparse;
use Carp;

use Test::More;

my $TEST_NAME = "";

sub assert(&) { my ($code) = @_;
    my $deparse = B::Deparse->new("-s");
    die("$TEST_NAME failed at " . $deparse->coderef2text($code) =~ s/\n//gr) unless $code->();
}

assert { chdir("test-repo/"); };

sub reset_repo() {
    assert { system("git checkout -f --no-track -B master origin/master") == 0; };
    assert { system("git clean -f -x -d") == 0; };
}

{
    $TEST_NAME = "smoke";
    reset_repo();
    assert { system("git reset --hard origin/b2") == 0; };
    assert { system("../git-rebase2 origin/b1") == 0; };
    assert { system("git diff --quiet origin/master") == 0; };
}

