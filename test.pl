#!/usr/bin/env perl

use strict;

use Data::Dumper;
use Carp;

use Test::More;

do 'git-rebase2';

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
# but this should work! - 2 is untouched and 1 is outer merge
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