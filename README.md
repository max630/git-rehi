# git-rebase2, advanced rebaser

This project is in early implementation stage. Most of the described is not
done or not even detailed.

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

## Why perl

Just because I like it. Also, it should be able be used with msysgit
installation out of the box.

## Todo list improvements

* ability to specify comment directly in the todo list
* native support for merges. Ability to specify comment, order of the children,
  also ours/theirs flag.
* merges with commits, which are not touched by the rebase. I call them
  external merges.
* merges with commits, which are touched by the rebase. I cal them internal
  merges. Design of this is a hard part. It should be able to specify the other
  commit paths which I do not intend to change, only pick verbatim, without
  listing them all. Maybe as ranges?

## Analysis improvements

Build reasonable todo list, with use all of the features listed in the previous
section. I see it as I specify the "from" and "to" commits, and optionally a
list of "through" commits, and it puts to the todo list the minimal path which
contains them.
