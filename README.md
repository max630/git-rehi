# git-rehi, history editor

Takes a history and reruns it, optionally with modifications.

Done to some usable state. See _Usage_ section later for reference.
Bug reports and wishes tracked here: https://github.com/max630/git-rehi

**NOTE**: Older perl version is [here](https://github.com/max630/git-rehi/tree/maint_0.5).

**SAFETY NOTES**: Note that it is an experimental software. You'd better have a
backup if you run it at a valuable repository, for example it should be cloned
somewhere. I also usually have some temporary reference to the older head and
inspect the result in gitk or otherwise before removing it. And never use
automatic gc (this script does not call it directly, but some of used commands
may trigger it if it is enabled in the config).

## Motivation

`git-rebase` is a wornderful tool, but it has a list of disadvantages:

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

## Installation

Recommended way:

* install [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
* run in the source tree: `stack build`
* last lines of output is something like:

    Installing executable(s) in
    C:\src\git-rehi\.stack-work\install\i386-linux\lts-3.17\7.10.2\bin

Copy it to a directory in your `PATH` and it is ready to use.

## Usage

Run the script inside of a git repository. If it is in PATH, git will run it
for you if you use `git rehi`.

`git rehi [options] <dest> [[<source_from>]..[<through1>..<through2>]..[<source_to>]] [<target>]`

Calculates a path (a) from `<source_from>` to `<source_to>`, and apply
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

`git rehi --continue`

Retry failed step after resolution. For some types of step this is not supported.
Then user have to manually apply the step and use `git rehi --skip` to continue.

`git rehi --abort`

Aborts whole reapply. The originally checked-out branch is checked out back.

`git rehi --skip`

Reset all uncommitted changes and continue reapplying starting with the next step in todo list.

`git rehi --current`

If there is a reapplying in progress, shows current step.

(a): the default path is some "optimal" which is not strictly specified and
subject to change. At some point it was the shortest. Now general rule is to
follow first parent but I might want to tune how it resolves ambiguous cases.

### Options

`--interactive`

Allows user to edit step list before starting applying them.

### Steps

Types of step, which `git-rehi` recognizes and which can be used in todo list.

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

`merge [--ours] -c <ahash> <ahash_1>,<ahash_2>,... [<subject>]`

Merge latest commit with others. Parents of the resulting commits will be
`ahash_1`, `ahash_2` and so on.  Their order exactly the same as specified in
the command. Exactly one of the `ahash_N` should be literal "`HEAD`" (without
quotes). If `--ours` is specified, merge will be performed with strategy `ours`
(that is, is will copy the `ahash_1`, just marking others as merged and
ignoring their contents).

Message for commit is taken from the `ahash` commit. `subject` is ignored.

`edit <ahash> [<subject>]`

Like `pick`, but stops after that, allowing user to make some manual changes.
Can be abbreviated to `e`.

`reset <ahash>`

Reset to the `ahash` commit and continue to apply following steps on top of it.

`: <mark>`

Remember current HEAD as a `mark`. Anywhere in the todo steps `ahash` can be
of form `@<mark>`. If it has been remembered before, the hash of the mark
will be used.

`exec <command>`

Executes a shell command. Can be abbreviated to `x`.

## Types of merge

* merge which contains only one parent from changes sequence, and all others
  already exist and are untouched by the reapplying. I call it "external merge".
  External merges are supported: detected and handled when specified in todo.
* merge which contains two or more parents from changes sequence. That is
  "internal merge". There is an experimental support for such merges, with use
  of `reset` and `:` steps.
