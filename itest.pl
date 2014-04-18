#!/usr/bin/env perl

use strict;

use Carp;
use Data::Dumper;
use Test::More;

my %Tests = ();
sub t(&*) { my ($block, $name) = @_;
    $Tests{$name} = $block;
}

foreach my $name (do { if (scalar @ARGV) { @ARGV } else { keys %Tests; }; }) {
    subtest $name => $Tests{$name};
}

done_testing();
