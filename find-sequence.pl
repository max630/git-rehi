#!/usr/bin/env perl

use strict;

use Data::Dumper;
use Carp;

use Test::More;
use Data::Compare;

# TODO:
# * through
# + explicitly forbid inner merges
# * inner merges
#  * interface
#  * extraction

# $commits : { $hash => { parents => [ $hash ] } }
sub find_sequence($$$$) { my ($commits, $from, $to, $through_list) = @_;
    my %through_hash = ();
    @through_hash{@{$through_list}} = ();
    my %commitsX = ();
    foreach my $h (keys %{$commits}) {
        $commitsX{$h} = { children_count => 0, children => [] };
    }
    $commitsX{$from} = { children_count => 0, children => [] };
    $commitsX{$to} = { children_count => 0, children => [], cost => [0, {}] };
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
                    $commitsX{$p}->{cost} = [$min_cost->[0] + 1, $min_cost->[1]];
                    if (exists $through_hash{$p}) {
                        my %new_set = $commitsX{$p}->{cost}->[1];
                        $new_set{$p} = ();
                        $commitsX{$p}->{cost}->[1] = \%new_set;
                    }
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
        if ($commitsX{$next}->{children_count} > 1) {
            confess("inner merges not supported yet (found in commit $next)");
        }
        push @res, $next;
    }
    return \@res;
}

sub find_minimal_cost(@) {
    my @result = (undef, undef);
    foreach my $item (@_) {
        if (!defined $result[0] || do {
                my ($result_cost, $result_marks) = @{$result[0]};
                my ($item_cost, $item_marks) = @{$item->[0]};
                if (is_subset($result_marks, $item_marks)) {
                    if (is_subset($item_marks, $result_marks)) {
                        # they are equal
                        $result_cost > $item_cost;
                    } else {
                        1;
                    }
                } else {
                    if (is_subset($item_marks, $result_marks)) {
                        0;
                    } else {
                        print Dumper($result_marks, $item);
                        confess("Non-subsequent throughs not supported");
                    }
                }
             }) {
            @result = @{$item};
        }
    }
    return @result;
}

sub is_subset(\%\%) { my ($sub, $super) = @_;
    foreach my $k (keys %$sub) {
        if (!exists $super->{$k}) {
            return 0;
        }
    }
    return 1;
}

sub mb_test() {
    my @c = caller;
    if ($0 ne $c[1]) {
        return 1;
    } else {
        is_deeply (find_sequence({ 1 => {parents => [2, 3]},
                                   2 => {parents => [3,5]}  },
                                   2, 1, []),
                                [1]);
        # 1 --- 2 --- 6
        #  \        /
        #   3 ---- 4
        is_deeply (find_sequence({ 1 => {parents => [2, 3]},
                                   2 => {parents => [6]},
                                   3 => {parents => [4]},
                                   4 => {parents => [6]},
                                   6 => {parents => [7,10]}  },
                                   6, 1, []),
                                [2,1]);
        # 1 --- 2 --- 6
        #  \        /
        #   3 ----4*
        is_deeply (find_sequence({ 1 => {parents => [2, 3]},
                                   2 => {parents => [6]},
                                   3 => {parents => [4]},
                                   4 => {parents => [6]},
                                   6 => {parents => [7,10]}  },
                                   6, 1, [4]),
                                [4,3,1]);
        # 1 --- 2*--- 6
        #  \        /
        #   3 ----4*
        isnt (do { eval { find_sequence({ 1 => {parents => [2, 3]},
                                   2 => {parents => [6]},
                                   3 => {parents => [4]},
                                   4 => {parents => [6]},
                                   6 => {parents => [7,10]}  },
                                   6, 1, [2,4]); }; $@; },
                                "");
        # 1 -- 2 -- 3 -- 4
        #  \       /    /
        #   5 --- 6 -- 7
        # should fail (inner merge in 1 from base 3 not allowed)
        isnt (do { eval {find_sequence({ 1 => {parents => [2, 5]},
                                         2 => {parents => [3]},
                                         3 => {parents => [4]},
                                         4 => {parents => []},
                                         5 => {parents => [6]},
                                         6 => {parents => [3, 7]},
                                         7 => {parents => [4]},
                                       }, 4, 1, [])}; $@;}, "");
        # 1 -- 2 -- 3 -- 4
        #  \       /    /
        #   5 --- 6 --7*
        # but this should work! - base for the 1 is 4
        is_deeply (find_sequence({ 1 => {parents => [2, 5]},
                                   2 => {parents => [3]},
                                   3 => {parents => [4]},
                                   4 => {parents => []},
                                   5 => {parents => [6]},
                                   6 => {parents => [3, 7]},
                                   7 => {parents => [4]},
                                  }, 4, 1, [7]),
                                [7,6,5,1]);
        done_testing();
    }
}

mb_test;
