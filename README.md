# git-rebase2, advanced rebaser

Done to some usable state. See the TODO.md for missing things.

## Motivation

The original rebase is a wornderful tool, but it has a list of disadvantages:

* Terribly slow at Windows. I guess it is because it is written in Bash.
* Very poor support for merges. They can be created with exec during interactive rebase,
  but when there are already merges, you don't get them back in the todo list.
* Inconvenient UI for reword. User first need to specify the todo list, then
  gets into sequence of resolving conflict, and at some point is finally able
  to write the comment. Would be much better to do that just during editing
  the todo list.

## Features

Features are follows from the list in previous section:

* Written in high-level language, with data structures and string manipulation
  without calling external programs.
* As few git calls as possible.
* Extended language for the todo list.
* Analysis before editing the todo list, selecting only the interesting path
  from the history to be changed.

## Usage

Run the script inside of a git repository. If it is in PATH, git will run it
for you if you use `git rebase2`.

In general, arguments similar to th original rebase's ones.

`git rebase2 [options] [--onto <newbase>] <upstream> [<branch>]`

Calculates optimal path from latest common ancestor of `<branch>` and
`<upstream>`, and apply it to repository, starting from `<newbase>`, or from
`<branch>`, if --onto is not specified. If `<branch>` is not specified,
the currently checked-out branch is used. In the end, `<branch>` is reset to
the top of the resulting commit sequence and checked out.

`git rebase2 --continue`

Retry failed step after resolution.

`git rebase2 --abort`

Aborts whole rebase. `<branch>` returns to where it was before starting and is
checked-out.

`git rebase2 --skip`

Skip failed step and go on with next ones.

### Options

`--interactive`

Allows user to edit step list before starting applying them.

`--through <ref>`

By default, `git-rebase2` looks for the shortest path from `<upstream>` to
`<base>`.  This option can be used to affect the path, specifying the commit
which it should contain. Can be used several times, specifying several commits.

### Steps

Types of step, which `git-rebase2` recognize and which can be used in todo list.

`pick <ahash> [<subject>]`

Apply the non-merge change at specified hash `ahash`, repeating it comment and
author. `subject` is ignored.

`fixup <ahash> [<subject>]`

Apply the non-merge change at specified hash `ahash` and amend the latest
commit which is currently in HEAD. No change to message. `subject` is ignored

`comment`

Change the message of the latest commit. Following lines are the message contents.
A line containing single "." marks the end of message.

`merge [--ours] <parent1> <parent2> ...`

Merge latest commit with others. Order of parents will be exactly the same as
specified in the command. Exactly one of the `parentN` should be literal
"`HEAD`" (without qquotes). If `--ours` is specified, merge will be performed
with strategy `ours` (that is, is will copy the `parent1`, just marking others
as merged and ignoring their contents).

Message for the merge commit follows the command, terminated with ".", like for
the `comment` command.

## Types of merge

* merge which contains only one parent from changes sequence, and all others
  already exist and are untouched by the rebase. I call it "external merge".
  They are supported, detected and handled when specified in todo.
* merge which contains two or more parents from changes sequence. That is
  "internal merge". Not supported currently. Could be manually constructed in
  todo if there were `exec` command.
