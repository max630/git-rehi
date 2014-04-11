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

is_deeply (read_todo(\<<End, []), [{ type => "pick", ahash => "12345"}]);
pick 12345
End
is_deeply (read_todo(\<<End, []), [{ type => "merge", parents => ["HEAD", "12345"], flags => {}, ahash => "93845345"}]);
merge -c 93845345 HEAD,12345 Test merge comment
End
is_deeply (read_todo(\<<End, []), [{ type => "merge", parents => ["HEAD", "12345"], flags => {}, ahash => "93845345"}]);
merge -c 93845345 HEAD,12345
End
sub { is_deeply (read_todo($_[0], []),
                   [{type => "pick",  ahash => "refs/rebase2/12345"},
                    {type => "fixup",  ahash => "refs/rebase2/12345"},
                    {type => "edit",  ahash => "refs/rebase2/12345"},
                    {type => "merge",  ahash => "refs/rebase2/12345", flags => {}, parents => ["HEAD", "45876", "refs/rebase2/ffeee12"]}]);
    }->(\<<End);
pick \@12345 Test comment
fixup \@12345 Test comment
edit \@12345 Test comment
merge -c \@12345 HEAD,45876,\@ffeee12 Test comment
End

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
         save_todo([{type => "pick",  ahash => "refs/rebase2/12345"},
                    {type => "fixup",  ahash => "refs/rebase2/12345"},
                    {type => "edit",  ahash => "refs/rebase2/12345"},
                    {type => "merge",  ahash => "refs/rebase2/12345", flags => {}, parents => ["HEAD", "45876", "refs/rebase2/ffeee12"]}],
                   \$out,
                   { refs => { "refs/rebase2/12345" => "12345ddd" },
                     by_hash => { "12345ddd" => { subject => "Test comment" } } });
         $out; },
    <<End);
pick \@12345 Test comment
fixup \@12345 Test comment
edit \@12345 Test comment
merge -c \@12345 HEAD,45876,\@ffeee12 Test comment
End

done_testing();
