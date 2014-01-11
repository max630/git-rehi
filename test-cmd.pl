#!/usr/bin/env perl

use strict;

use Data::Dumper;
use B::Deparse;
use Carp;

my $TEST_NAME = "";

sub assert(&) { my ($code) = @_;
    my $deparse = B::Deparse->new("-s");
    die("$TEST_NAME failed at " . $deparse->coderef2text($code) =~ s/\n//gr) unless $code->();
}

{
    $TEST_NAME = "smoke";
    #assert { 0; };
}
