# git-rebase2, advanced rebaser

Done to some usable state. See _Usage_ section later for reference.
Bug reports and wishes tracked here: https://github.com/max630/git-rebase2

**SAFETY NOTES**: Note that it is an experimental software. You'd better have a
backup if you run it at a valuable repository, for example it should be cloned
somewhere. I also usually have some temporary reference to the older head and
inspect the result in gitk or otherwise before removing it. And never use
automatic gc (this script does not call it directly, but some of used commands
may trigger it if it is enabled in the config).

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

List of features

* Written in high-level language, with data structures and string manipulation
  without calling external programs.
* Should work on Windows with msysgit only, without installing additoinal software.
* As few git calls as possible.
* Extended language for the todo list.
* Analysis before editing the todo list, selecting only the interesting path
  from the history to be changed.

## Usage

Run the script inside of a git repository. If it is in PATH, git will run it
for you if you use `git rebase2`.

`git rebase2 [options] <dest> [[<source_from>]..[<through1>..<through2>]..[<source_to>]] [<target>]`

Calculates the shortest path from `<source_from>` to `<source_to>`, and apply
it to repository, starting from `<dest>`. In the end, `<target>` branch is
reset to the top of the resulting commit sequence and checked out.

Optionally between `<source_from>` and `<source_to>` can be one or more
`<through>` revisions. Then the source path is adjusted so that it contains all
of these revisions.

If `<source_from>` is omitted, the latest common ancestor of `<dest>` and
`<source_to>` is used instead.  If `<source_to>` is omitted, `<target>` is used
instead.  If `<target>` is omitted, the currently checked-out branch is used.
If the whole source argument `<from>.. ..<to>` is omitted, it is equivalent to
specification "`..`".

`git rebase2 --continue`

Retry failed step after resolution. For some types of step this is not supported.
Then user have to manually apply the step and use `git rebase2 --skip` to continue.

`git rebase2 --abort`

Aborts whole rebase. The originally checked-out branch is checked out back.

`git rebase2 --skip`

Reset all uncommitted changes and continue rebasing starting with the next step in todo list.

### Options

`--interactive`

Allows user to edit step list before starting applying them.

### Steps

Types of step, which `git-rebase2` recognizes and which can be used in todo list.

`pick <ahash> [<subject>]`

Apply the non-merge change at specified hash `ahash`, repeating it comment and
author. `subject` is ignored. Can be abbreviated to `p`.

`fixup <ahash> [<subject>]`

Apply the non-merge change at specified hash `ahash` and amend the latest
commit which is currently in HEAD. No change to message. `subject` is ignored.
Can be abbreviated to `f`.

`comment`

Change the message of the latest commit. Following lines are the message contents.
A line containing single "." marks the end of message.

`merge [--ours] <parent1> <parent2> ...`

Merge latest commit with others. Order of parents will be exactly the same as
specified in the command. Exactly one of the `parentN` should be literal
"`HEAD`" (without quotes). If `--ours` is specified, merge will be performed
with strategy `ours` (that is, is will copy the `parent1`, just marking others
as merged and ignoring their contents).

Message for the merge commit follows the command, terminated with ".", like for
the `comment` command.

`edit <ahash> [<subject>]`

Like `pick`, but stops after that, allowing user to make some manual changes.
Can be abbreviated to `e`.

`exec <command>`

Executes a shell command. Can be abbreviated to `x`.

## Types of merge

* merge which contains only one parent from changes sequence, and all others
  already exist and are untouched by the rebase. I call it "external merge".
  They are supported, detected and handled when specified in todo.
* merge which contains two or more parents from changes sequence. That is
  "internal merge". Not supported currently, path finding aborts if
  they are detected.
  But they still can be manually constructed in interactive rebase with using
  step `exec`, like in original rebase.
