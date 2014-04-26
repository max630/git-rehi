#!/usr/bin/env perl

use strict;

use Data::Dumper;
use Carp;

use Test::More;

do 'git-rebase2';


my %Tests = ();
sub t(&*) { my ($block, $name) = @_;
    $Tests{$name} = $block;
}

t { is_deeply (find_sequence({ 1 => {parents => [2]},
                                        2 => {parents => [3]},
                                        3 => {parents => [4]},},
                                        4, 1, []),
                         [3, 2, 1]); } linear;
# 1 -- 2
#  \    \
#   3 -- 4 - 5
t { is_deeply (find_sequence({ 1 => {parents => [2, 3]},
                           2 => {parents => [4]},
                           3 => {parents => [4]},
                           4 => {parents => [5]},},
                           5, 1, []),
                        [4, 3, 2, 1]) } diamond;

t { is_deeply (find_sequence({ 1 => {parents => [2, 3]},
                           2 => {parents => [3,5]}  },
                           2, 1, []),
                        [1]); } simple_branch;

# 1 --- 2 --- 6
#  \        /
#   3 ---- 4
t { is_deeply (find_sequence({ 1 => {parents => [2, 3]},
                           2 => {parents => [6]},
                           3 => {parents => [4]},
                           4 => {parents => [6]},
                           6 => {parents => [7,10]}  },
                           6, 1, []),
                        [2,1]); } shortest;
# 1 --- 2 --- 6
#  \        /
#   3 ----4*
t { is_deeply (find_sequence({ 1 => {parents => [2, 3]},
                           2 => {parents => [6]},
                           3 => {parents => [4]},
                           4 => {parents => [6]},
                           6 => {parents => [7,10]}  },
                           6, 1, [4]),
                        [4,3,1]); } through;
# 1 --- 2*--- 6
#  \        /
#   3 ----4*
t { isnt (do { eval { find_sequence({ 1 => {parents => [2, 3]},
                           2 => {parents => [6]},
                           3 => {parents => [4]},
                           4 => {parents => [6]},
                           6 => {parents => [7,10]}  },
                           6, 1, [2,4]); }; $@; },
                        ""); } parallel_throughs;
# 1 -- 2 -- 3 -- 4
#  \       /    /
#   5 --- 6 -- 7
t { is_deeply (find_sequence({ 1 => {parents => [2, 5]},
                                 2 => {parents => [3]},
                                 3 => {parents => [4]},
                                 4 => {parents => []},
                                 5 => {parents => [6]},
                                 6 => {parents => [3, 7]},
                                 7 => {parents => [4]},
                               }, 4, 1, []),
                [3, 6, 5, 2, 1]); } inner_merge;
# 1 -- 2 -- 3 -- 4
#  \       /    /
#   5 --- 6 --7*
# but this should work! - 2 is untouched and 1 is outer merge
t { is_deeply (find_sequence({ 1 => {parents => [2, 5]},
                           2 => {parents => [3]},
                           3 => {parents => [4]},
                           4 => {parents => []},
                           5 => {parents => [6]},
                           6 => {parents => [3, 7]},
                           7 => {parents => [4]},
                          }, 4, 1, [7]),
                        [7,6,5,1]); } inner_merge_through_wa;

t {
is_deeply (parse_cli(['a']), ['RUN', 'a', undef, [], undef, undef, 0]);
is_deeply (parse_cli(['a', 'c']), ['RUN', 'a', undef, [], undef, 'c', 0]);
is_deeply (parse_cli(['a', 'b..d', 'c']), ['RUN', 'a', 'b', [], 'd', 'c', 0]);
is_deeply (parse_cli(['a', 'b..', 'c']), ['RUN', 'a', 'b', [], '', 'c', 0]);
is_deeply (parse_cli(['a', '..d', 'c']), ['RUN', 'a', '', [], 'd', 'c', 0]);
is_deeply (parse_cli(['a', 'b..e..d', 'c']), ['RUN', 'a', 'b', ['e'], 'd', 'c', 0]);
is_deeply (parse_cli(['a', '..e..', 'c']), ['RUN', 'a', '', ['e'], '', 'c', 0]);
is_deeply (parse_cli(['a', '..e..']), ['RUN', 'a', '', ['e'], '', undef, 0]);
is_deeply (parse_cli(['a', 'b..e..f..d', 'c']), ['RUN', 'a', 'b', ['e', 'f'], 'd', 'c', 0]);

isnt (do { eval { parse_cli(['a', 'b...d', 'c']) }; $@ }, '');
isnt (do { eval { parse_cli(['a', 'b....d', 'c']) }; $@ }, '');
} parse_cli;

t {
is_deeply (read_todo(\<<End, []), [{ type => "pick", ahash => "12345"}]);
pick 12345
End
is_deeply (read_todo(\<<End, []), [{ type => "pick", ahash => "origin/master"}]);
pick origin/master
End
is_deeply (read_todo(\<<End, []), [{ type => "merge", parents => ["HEAD", "12345"], flags => {}, ahash => "93845345"}]);
merge -c 93845345 HEAD,12345 Test merge comment
End
is_deeply (read_todo(\<<End, []), [{ type => "merge", parents => ["HEAD", "12345"], flags => {}, ahash => "93845345"}]);
merge -c 93845345 HEAD,12345
End
sub { is_deeply (read_todo($_[0], []),
                   [{type => "mark", name => "12345"},
                    {type => "pick",  ahash => "\@12345"},
                    {type => "fixup",  ahash => "\@12345"},
                    {type => "edit",  ahash => "\@12345"},
                    {type => "reset",  ahash => "\@12345"},
                    {type => "merge",  ahash => "\@12345", flags => {}, parents => ["HEAD", "45876", "\@ffeee12"]}]);
    }->(\<<End);
: 12345
pick \@12345 Test comment
fixup \@12345 Test comment
edit \@12345 Test comment
reset \@12345
merge -c \@12345 HEAD,45876,\@ffeee12 Test comment
End
} read_todo;

t {
is (do { my $out;
         save_todo([{ type => "pick",  ahash => "12345"}],
                   \$out,
                   { refs => { "12345" => "12345ddd" },
                     by_hash => { "12345ddd" => { subject => "Test comment" } } });
         $out; },
    <<End);
pick 12345 Test comment
End
is (do { my $out;
         save_todo([{ type => "merge", parents => ["HEAD", "12345"], flags => {}, ahash => "93845345"}],
                   \$out,
                   { refs => { "93845345" => "93845345aaa" },
                     by_hash => { "93845345aaa" => { subject => "Test comment" } } });
         $out; },
    <<End);
merge -c 93845345 HEAD,12345 Test comment
End
is (do { my $out;
         save_todo([{type => "mark", name => "12345"},
                    {type => "pick",  ahash => "\@12345"},
                    {type => "fixup",  ahash => "\@12345"},
                    {type => "edit",  ahash => "\@12345"},
                    {type => "reset",  ahash => "\@12345"},
                    {type => "merge",  ahash => "\@12345", flags => {}, parents => ["HEAD", "45876", "\@ffeee12"]}],
                   \$out,
                   { refs => { "\@12345" => "12345ddd" },
                     by_hash => { "12345ddd" => { subject => "Test comment" } } });
         $out; },
    <<End);
: 12345
pick \@12345 Test comment
fixup \@12345 Test comment
edit \@12345 Test comment
reset \@12345
merge -c \@12345 HEAD,45876,\@ffeee12 Test comment
End
} save_todo;

t {
is (do { my $out;
         save_todo([{ type => "merge", parents => ["HEAD", "12345"], flags => {} }],
                   \$out,
                   { refs => { },
                     by_hash => { } });
         $out; },
    <<End);
merge HEAD,12345
End
is_deeply (read_todo(\<<End, []), [{ type => "merge", parents => ["HEAD", "12345"], flags => {} }]);
merge HEAD,12345
End
is_deeply (read_todo(\<<End, []), [{ type => "merge", parents => ["HEAD", "12345"], flags => {} }]);
merge HEAD,12345 Some subject
End
} merge_no_c;

foreach my $name (do { if (scalar @ARGV) { @ARGV } else { keys %Tests; }; }) {
    subtest $name => $Tests{$name};
}

done_testing();
