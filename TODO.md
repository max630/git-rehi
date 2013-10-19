

## Known issues

Should be fixed.

* missing documentation
* handle errors more nicely. I like stacktraces, but they should not popup in everyday situations like merge conflict.
* less noise in general
* parse --continue, --skip and --abort as separated option from regular call
* when detecting merge, check for equal trees with other parents also. Otherwise information can be lost
* --through which was not met should be an error

## Nice to have

I pretend the script to be useful as is, but more features is always better

* 'exec' step in todo. Needed for creating inner merges at least.
* inner merges handling. Was planned from beginning, and should not be very hard in backend, but I still don't have an idea how to make acceptable UI.
* syntax-check of todo with retry.
* 'request-comment' for editing in todo.
