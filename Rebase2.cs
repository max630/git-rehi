using System;
using System.Collections.Generic;

namespace rebase2 {
    public class Rebase2 {
        public sealed struct Commits {
            IDictionary<string, string>
        }

        public sealed struct Commit {
            string ahash;
            string hash;
            string subject;
            IEnumerable<string> parents;
        }

        public abstract class Step {
            private sealed class Pick : Step {
                string ahash;
                public T Match<T>(
                    Func<string, T> pick,
                    Func<string, T> fixup,
                    Func<string, T> edit,
                    Func<string, T> comment,
                    Func<string, T> exec,
                    Func<IEnumerable<string>, string, bool, T> merge) override { return pick(ahash); }
            }
            private sealed class Fixup : Step {
                string ahash;
                public T Match<T>(
                    Func<string, T> pick,
                    Func<string, T> fixup,
                    Func<string, T> edit,
                    Func<string, T> comment,
                    Func<string, T> exec,
                    Func<IEnumerable<string>, string, bool, T> merge) override { return fixup(ahash); }
            }
            private sealed class Edit : Step {
                string ahash;
                public T Match<T>(
                    Func<string, T> pick,
                    Func<string, T> fixup,
                    Func<string, T> edit,
                    Func<string, T> comment,
                    Func<string, T> exec,
                    Func<IEnumerable<string>, string, bool, T> merge) override { return edit(ahash); }
            }
            private sealed class Comment : Step {
                string _comment;
                public T Match<T>(
                    Func<string, T> pick,
                    Func<string, T> fixup,
                    Func<string, T> edit,
                    Func<string, T> comment,
                    Func<string, T> exec,
                    Func<IEnumerable<string>, string, bool, T> merge) override { return comment(_comment); }
            }
            private sealed class Exec : Step {
                string _command;
                public T Match<T>(
                    Func<string, T> pick,
                    Func<string, T> fixup,
                    Func<string, T> edit,
                    Func<string, T> comment,
                    Func<string, T> exec,
                    Func<IEnumerable<string>, string, bool, T> merge) override { return exec(_command); }
            }
            private sealed class Merge : Step {
                IEnumerable<string> _parents;
                string _comment;
                bool _ours;
                public T Match<T>(
                    Func<string, T> pick,
                    Func<string, T> fixup,
                    Func<string, T> edit,
                    Func<string, T> comment,
                    Func<string, T> exec,
                    Func<IEnumerable<string>, string, bool, T> merge) override
                { 
                    return pick(_parents, _comment, _ours);
                }
            }

            static Step NewPick(string ahash_) { return new Pick() { ahash = ahash_ }; }
            static Step NewFixup(string ahash_) { return new Fixup() { ahash = ahash_ }; }
            static Step NewEdit(string ahash_) { return new Edit() { ahash = ahash_ }; }
            static Step NewComment(string comment) { return new Comment() { _comment = comment }; }
            static Step NewExec(string command) { return new Exec() { _command = command }; }
            static Step NewMerge(IEnumerable<string> parents, string comment, bool ours) { return new Merge() {
                _parents = parents,
                _comment = comment,
                _ours = ours
            }; }

            public abstract T Match<T>(
                Func<string, T> pick,
                Func<string, T> fixup,
                Func<string, T> edit,
                Func<string, T> comment,
                Func<string, T> exec,
                Func<IEnumerable<string>, string, bool, T> merge);
        }
    }
}
