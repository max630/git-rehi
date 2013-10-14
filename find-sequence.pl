#!/usr/bin/env perl

use strict;

use Data::Dumper;
use Carp;

use Test::More;
use Data::Compare;

# TODO:
# * through
# * inner merges
#  * interface
#  * extraction

# $commits : { $hash => { parents => [ $hash ] } }
sub find_sequence($$$) { my ($commits, $from, $to) = @_;
    my %commitsX = ();
    foreach my $h (keys %{$commits}) {
        $commitsX{$h} = { children_count => 0, children => [] };
    }
    $commitsX{$from} = { children_count => 0, children => [] };
    $commitsX{$to} = { children_count => 0, children => [], cost => 0 };
    foreach my $h (keys %{$commits}) {
        foreach my $p (@{$commits->{$h}->{parents}}) {
            if (exists $commitsX{$p}) {
                $commitsX{$p}->{children_count}++;
            }
        }
    }
    my %edge = ($to => 1);
    while (scalar %edge) {
        my %next_edge = ();
        foreach my $v (keys %edge) {
            my $vertex_cost = $commitsX{$v}->{cost};
            foreach my $p (@{$commits->{$v}->{parents}}) {
                push(@{$commitsX{$p}->{children}}, $v);
                if (scalar @{$commitsX{$p}->{children}} == $commitsX{$p}->{children_count}) {
                    my ($min_cost, $min_child) = find_minimal_cost(map { [$commitsX{$_}->{cost}, $_] } @{$commitsX{$p}->{children}});
                    $commitsX{$p}->{cost} = $min_cost + 1;
                    $commitsX{$p}->{sequence_child} = $min_child;
                    $next_edge{$p} = 1;
                }
            }
        }
        %edge = %next_edge;
    }
    my @res = ();
    my $next = $from;
    while ($next ne $to) {
        $next = $commitsX{$next}->{sequence_child};
        push @res, $next;
    }
    return \@res;
}

sub find_minimal_cost(@) {
    my @result = (undef, undef);
    foreach my $item (@_) {
        if (!defined $result[0] || $result[0] > $item->[0]) {
            @result = @{$item};
        }
    }
    return @result;
}

sub mb_test() {
    my @c = caller;
    if ($0 ne $c[1]) {
        return 1;
    } else {
        is_deeply (find_sequence({ 1 => {parents => [2, 3]},
                                   2 => {parents => [3,5]}  },
                                   2, 1),
                                [1]);
        # 1 --- 2 --- 6
        #  \        /
        #   3 ---- 4
        is_deeply (find_sequence({ 1 => {parents => [2, 3]},
                                   2 => {parents => [6]},
                                   3 => {parents => [4]},
                                   4 => {parents => [6]},
                                   6 => {parents => [7,10]}  },
                                   6, 1),
                                [2,1]);
        done_testing();
    }
}

mb_test;
