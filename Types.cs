using System;
using System.Collections.Generic;

namespace rebase2 {
    public class Types {
        public struct Commits {
            public IDictionary<string, Commit> byAHash;
            public IDictionary<string, Commit> byHash;
        }

        public struct Commit {
            public string ahash;
            public string hash;
            public string subject;
            public IEnumerable<string> parents;
        }

        public abstract class Step {
            private sealed class Pick : Step {
                internal string ahash;

                public override T Match<T>(
                    Func<string, T> pick,
                    Func<string, T> fixup,
                    Func<string, T> edit,
                    Func<string, T> comment,
                    Func<string, T> exec,
                    Func<IEnumerable<string>, string, bool, T> merge)
                {
                    return pick(ahash);
                }
            }

            private sealed class Fixup : Step {
                internal string ahash;

                public override T Match<T>(
                    Func<string, T> pick,
                    Func<string, T> fixup,
                    Func<string, T> edit,
                    Func<string, T> comment,
                    Func<string, T> exec,
                    Func<IEnumerable<string>, string, bool, T> merge)
                {
                    return fixup(ahash);
                }
            }

            private sealed class Edit : Step {
                internal string ahash;

                public override T Match<T>(
                    Func<string, T> pick,
                    Func<string, T> fixup,
                    Func<string, T> edit,
                    Func<string, T> comment,
                    Func<string, T> exec,
                    Func<IEnumerable<string>, string, bool, T> merge)
                {
                    return edit(ahash);
                }
            }

            private sealed class Comment : Step {
                internal string _comment;

                public override T Match<T>(
                    Func<string, T> pick,
                    Func<string, T> fixup,
                    Func<string, T> edit,
                    Func<string, T> comment,
                    Func<string, T> exec,
                    Func<IEnumerable<string>, string, bool, T> merge)
                {
                    return comment(_comment);
                }
            }

            private sealed class Exec : Step {
                internal string _command;

                public override T Match<T>(
                    Func<string, T> pick,
                    Func<string, T> fixup,
                    Func<string, T> edit,
                    Func<string, T> comment,
                    Func<string, T> exec,
                    Func<IEnumerable<string>, string, bool, T> merge)
                {
                    return exec(_command);
                }
            }

            private sealed class Merge : Step {
                internal IEnumerable<string> _parents;
                internal string _comment;
                internal bool _ours;

                public override T Match<T>(
                    Func<string, T> pick,
                    Func<string, T> fixup,
                    Func<string, T> edit,
                    Func<string, T> comment,
                    Func<string, T> exec,
                    Func<IEnumerable<string>, string, bool, T> merge)
                { 
                    return merge(_parents, _comment, _ours);
                }
            }

            public static Step NewPick(string ahash_)
            {
                return new Pick() { ahash = ahash_ };
            }

            public static Step NewFixup(string ahash_)
            {
                return new Fixup() { ahash = ahash_ };
            }

            public static Step NewEdit(string ahash_)
            {
                return new Edit() { ahash = ahash_ };
            }

            public static Step NewComment(string comment)
            {
                return new Comment() { _comment = comment };
            }

            public static Step NewExec(string command)
            {
                return new Exec() { _command = command };
            }

            public static Step NewMerge(IEnumerable<string> parents, string comment, bool ours)
            {
                return new Merge() {
                    _parents = parents,
                    _comment = comment,
                    _ours = ours
                };
            }

            public abstract T Match<T>(
                Func<string, T> pick,
                Func<string, T> fixup,
                Func<string, T> edit,
                Func<string, T> comment,
                Func<string, T> exec,
                Func<IEnumerable<string>, string, bool, T> merge);

            public void Match(
                Action<string> pick,
                Action<string> fixup,
                Action<string> edit,
                Action<string> comment,
                Action<string> exec,
                Action<IEnumerable<string>, string, bool> merge)
            {
                Match<int>(
                    pick: x => { pick(x); return 0; },
                    edit: x => { edit(x); return 0; },
                    fixup: x => { fixup(x); return 0; },
                    comment: x => { comment(x); return 0; },
                    exec: x => { exec(x); return 0; },
                    merge: (x,y,z) => { merge(x,y,z); return 0; });
            }
        }
    }
}
