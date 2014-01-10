using System;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Collections.Generic;

namespace rebase2 {
    public class Rebase2 {
        public static void Main(string[] args)
        {
            bool isInteractive = false;
            string ontoRef = null;
            var through = new List<string>();
            int offset = 0;
            while (offset < args.Length) {
                if (args[offset].Equals("-i") || args[offset].Equals("--interactive")) {
                    isInteractive = true;
                    offset++;
                } else if (args[offset].Equals("--through")) {
                    through.Add(args[offset + 1]);
                    offset += 2;
                } else if (args[offset].Equals("--onto")) {
                    if (ontoRef != null)
                        throw new Exception("--onto doubled");
                    ontoRef = args[offset + 1];
                    offset += 2;
                } else if (args[offset].Equals("--abort")) {
                    if (offset + 1 < args.Length)
                        throw new Exception(String.Format("Extra arguments after {0}: {1}", args[offset], String.Join(", ", Enumerable.Skip(args, offset + 1))));
                    // abortRebase();
                    return;
                } else if (args[offset].Equals("--skip")) {
                    if (offset + 1 < args.Length)
                        throw new Exception(String.Format("Extra arguments after {0}: {1}", args[offset], String.Join(", ", Enumerable.Skip(args, offset + 1))));
                    Utils.Let(
                        restoreRebase(),
                        (todo, current, commits, target_ref) => {
                            if (current != null) {
                                IOUtils.Run("git reset --hard HEAD");
                                File.Delete(Path.Combine(Environment.GitDir, "rebase2", "current"));
                            }
                            run_rebase(todo, commits, target_ref); });
                    return;
                } else if (args[offset].Equals("--continue")) {
                    if (offset + 1 < args.Length)
                        throw new Exception(String.Format("Extra arguments after {0}: {1}", args[offset], String.Join(", ", Enumerable.Skip(args, offset + 1))));
                    Utils.Let(
                        restoreRebase(),
                        (todo, current, commits, target_ref) => {
                            if (current != null) {
                                run_continue(current, commits);
                                File.Delete(RebasePath("current"));
                            }
                            run_rebase(todo, commits, target_ref); });
                    return;
                } else if (offset + 1 == args.Length) {
                    GitUtils.verifyClean();
                    var headRef = IOUtils.ReadPopen("git", "symbolic-ref -q HEAD");
                    Match match = null;
                    if ((match = Regex.Match(headRef, @"^refs/heads/(.*)")).Success)
                        mainParsed(args[offset], match.Groups[1].Value, ontoRef, through, isInteractive);
                    else
                        throw new Exception(String.Format("Unsupported ref checked-out: {0}", headRef));
                    return;
                } else if (offset + 2 == args.Length) {
                    GitUtils.verifyClean();
                    mainParsed(args[offset], args[offset + 1], ontoRef, through, isInteractive);
                    return;
                } else {
                    throw new Exception(String.Format("Invalid arguments: {0}", String.Join(", ", Enumerable.Skip(args, offset))));
                }
            }
        }

        public static void mainParsed(string dest, string source_to, string onto, IEnumerable<string> through, bool isInteractive)
        {
            var source_from = GitUtils.mergeBase(dest, source_to);
            var real_dest = (onto != null) ? onto : dest;
            Utils.Let(
                init_rebase(real_dest, source_from, source_to, through),
                (todo, commits, target_ref, dest_hash) => {
                    if (isInteractive) {
                        bool isOk;
                        todo = new List<Types.Step>(editTodo(todo, commits, out isOk));
                        if (!isOk) {
                            cleanup_save();
                            Console.WriteLine("Aborted");
                            return;
                        }
                    }
                    if (todo.Count > 0) {
                        IOUtils.Run(String.Format("git checkout --quiet --detach {0}", dest_hash));
                        run_rebase(todo, commits, target_ref);
                    } else {
                        cleanup_save();
                        Console.WriteLine("Nothing to do.");
                    }
                });
        }

        static Tuple<List<Types.Step>, Types.Step, Types.Commits, string> restoreRebase()
        {
            string targetRef = Enumerable.Single<string>(File.ReadLines(RebasePath("target_ref")));
            var commits = loadCommits();
            var todo = readTodo(RebasePath("target_ref"), commits).Item1;
            Types.Step current = File.Exists(RebasePath("current")) ? Enumerable.Single<Types.Step>(readTodo(RebasePath("current"), commits).Item1) : null;
            return Tuple.Create(todo, current, commits, targetRef);
        }

        static Tuple<List<Types.Step>, Types.Commits, string, string> init_rebase(string dest, string source_from, string source_to, IEnumerable<string> through)
        {
            Console.Error.WriteLine("initRebase: {0}, {1}, {2}, {3}", dest, source_from, source_to, through);
            string target_ref = source_to;
            return Utils.Let(
                GitUtils.resolveHashes(new List<string> { dest, source_from, source_to }),
                (dest_hash, source_from_hash, source_to_hash) => {
                    var throughHashes = new List<string>(GitUtils.resolveHashes(through));
                    init_save(target_ref);
                    var commits = fetch_commits(source_from, source_to);
                    var todo = build_rebase_sequence(commits, source_from_hash, source_to_hash, throughHashes);
                    return Tuple.Create(todo, commits, target_ref, dest_hash); });
        }

        static List<Types.Step> build_rebase_sequence(Types.Commits commits, string source_from, string source_to, ICollection<string> throughHashes)
        {
            throw new NotImplementedException();
        }

        static Types.Commits fetch_commits(string source_from, string source_to)
        {
            IOUtils.verify_cmdarg(source_from);
            IOUtils.verify_cmdarg(source_to);
            var commits = new Types.Commits();
            foreach (var line in IOUtils.EnumPopen("git", String.Format("log --ancestry-path --pretty=format:%H:%h:%P:%s {0}..{1}", source_from, source_to))) {
                parse_commit_line(commits, line);
            }
            return commits;
        }

        static void parse_commit_line(Types.Commits commits, string line)
        {
            Match match = Regex.Match(line, @"^([0-9a-f]+):([0-9a-f]+):([0-9a-f ]*):(.*)$");
            if (!match.Success)
                throw new Exception(String.Format("Invalid commit line: {0}", line));
            var commit = new Types.Commit() {
                hash = match.Groups[1].Value,
                ahash = match.Groups[2].Value,
                subject = match.Groups[4].Value,
                parents = Regex.Split(match.Groups[3].Value, " ") };
            GitUtils.verify_hash(commit.hash);
            if (commits.byAHash.ContainsKey(commit.ahash) || commits.byHash.ContainsKey(commit.hash))
                throw new Exception(String.Format("Diplicated commit: {0} ({1})", commit.hash, commit.ahash));
            commits.byAHash.Add(commit.ahash, commit);
            commits.byHash.Add(commit.hash, commit);
        }

        static void init_save(string target_ref)
        {
            if(File.Exists(RebaseDir()))
                throw new Exception("already in progress?");
            Directory.CreateDirectory(RebaseDir());
            File.WriteAllText(RebasePath("target_ref"), target_ref);
        }

        static void cleanup_save()
        {
            Directory.Delete(RebaseDir(), true);
        }

        static void run_continue(Types.Step step, Types.Commits commits)
        {
            IOUtils.Run("git rev-parse --verify HEAD >/dev/null"
                        + " && git update-index --ignore-submodules --refresh"
                        + " && git diff-files --quiet --ignore-submodules");
            step.Match(
                pick: hash => {
                    if (!GitUtils.getNoUncommittedChanges())
                        IOUtils.Run(String.Format("git commit -c {0}", hash));
                },
                edit: hash => {
                    if (!GitUtils.getNoUncommittedChanges())
                        throw new Exception("No unstaged changes should be after 'edit'");
                },
                fixup: hash => {
                    if (!GitUtils.getNoUncommittedChanges())
                        IOUtils.Run("git commit --amend");
                },
                comment: comment => { do_comment(comment); },
                exec: command => { throw new Exception(String.Format("Cannot continue 'exec {0}'; resolve it manually, then skip or abort", command)); },
                merge: (parents, comment, isOurs) => { throw new Exception("Merge continuation not implemented"); }
                );
            throw new NotImplementedException();
        }

        static void run_rebase(IEnumerable<Types.Step> todo, Types.Commits commits, string target_ref)
        {
            throw new NotImplementedException();
        }

        static void do_comment(object comment)
        {
            throw new NotImplementedException();
        }

        class InvalidTodoException : Exception
        {
            public InvalidTodoException(string msg) : base(msg) { }
        }

        static IEnumerable<Types.Step> editTodo(List<Types.Step> oldTodo, Types.Commits commits, out bool isOk)
        {
            var TmpFile = IOUtils.MakeTempFile(".todo.txt");
            isOk = false;
            try {
                saveTodo(oldTodo, TmpFile, commits);
                var Editor = GitUtils.sequence_editor();
                var NewData = Utils.Retry<List<Types.Step>, InvalidTodoException>(
                    () => {
                        // TODO: here could be older approach useful
                        IOUtils.Run(Editor + " " + TmpFile);
                        return readTodo(TmpFile, commits, msg => { throw new InvalidTodoException(msg); }).Item1;
                    }
                );
                isOk = true;
                return NewData;
            } finally {
                File.Delete(TmpFile);
            }
        }

        public static Types.Commits loadCommits()
        {
            throw new NotImplementedException();
        }

        public static void saveTodo(IEnumerable<Types.Step> Todo, string todoFile, Types.Commits commits)
        {
            using (var Out = File.CreateText(todoFile)) {
                foreach (var step in Todo) {
                    step.Match<int>(
                        pick: ahash => { Out.WriteLine("pick {0} {1}", ahash, commits.byAHash[ahash].subject); return 0; },
                        fixup: ahash => { Out.WriteLine("fixup {0} {1}", ahash, commits.byAHash[ahash].subject); return 0; },
                        edit: ahash => { Out.WriteLine("edit {0} {1}", ahash, commits.byAHash[ahash].subject); return 0; },
                        comment: comment => {
                            Out.WriteLine("comment");
                            foreach (var line in comment)
                                if (!line.Equals("."))
                                    Out.WriteLine("{0}", line);
                            Out.WriteLine(".");
                            return 0;
                        },
                        exec: command => {
                            if (command.Contains("\n"))
                                throw new Exception(String.Format("Multiline command cannot be saved: {0}", command));
                            Out.WriteLine("exec {0}", command);
                            return 0;
                        },
                        merge: (parents, comment, isOurs) => {
                            Out.WriteLine("merge{0} {1}",
                                            isOurs ? " --ours" : "",
                                            String.Join(" ", parents));
                            foreach (var line in comment)
                                if (!line.Equals("."))
                                    Out.WriteLine("{0}", line);
                            Out.WriteLine(".");
                            return 0;
                        }
                    );
                }
            }
        }

        enum Mode {
            COMMAND,
            COMMENT,
            MERGE_COMMENT,
        }

        public static Tuple<List<Types.Step>, List<string>> readTodo(string Filename, Types.Commits commits)
        {
            return readTodo(Filename, commits, msg => { throw new Exception(msg); });
        }

        public static Tuple<List<Types.Step>, List<string>> readTodo(string Filename, Types.Commits commits, Action<string> onSyntaxError)
        {
            var Todo = new List<Types.Step>();
            var unknownCommits = new List<string>();
            Mode mode = Mode.COMMAND;
            string comment = "";
            List<string> merge_parents = null;
            bool merge_ours = false;
            foreach (var todoLine in File.ReadLines(Filename)) {
                Match match;
                if (Regex.IsMatch(todoLine, @"^\s*#"))
                    continue;
                switch (mode) {
                    case Mode.COMMAND:
                        if ((match = Regex.Match(todoLine, @"^(f|fixup) ([0-9a-f]+)( (.*))?$")).Success) {
                            Todo.Add(Types.Step.NewFixup(match.Groups [2].Value));
                            if (!commits.byAHash.ContainsKey(match.Groups [2].Value))
                                unknownCommits.Add(match.Groups [2].Value);
                        } else if ((match = Regex.Match(todoLine, @"^(p|pick) ([0-9a-f]+)( (.*))?$")).Success) {
                            Todo.Add(Types.Step.NewPick(match.Groups [2].Value));
                            if (!commits.byAHash.ContainsKey(match.Groups [2].Value))
                                unknownCommits.Add(match.Groups [2].Value);
                        } else if ((match = Regex.Match(todoLine, @"^(e|edit) ([0-9a-f]+)( (.*))?$")).Success) {
                            Todo.Add(Types.Step.NewEdit(match.Groups [2].Value));
                            if (!commits.byAHash.ContainsKey(match.Groups [2].Value))
                                unknownCommits.Add(match.Groups [2].Value);
                        } else if ((match = Regex.Match(todoLine, @"^(x|exec) (.+)$")).Success) {
                            Todo.Add(Types.Step.NewExec(match.Groups [2].Value));
                        } else if ((match = Regex.Match(todoLine, @"^comment\b$")).Success) {
                            mode = Mode.COMMENT;
                            comment = "";
                        } else if ((match = Regex.Match(todoLine, @"^merge( --ours)? (.+[^\n\r ])$")).Success) {
                            mode = Mode.MERGE_COMMENT;
                            comment = "";
                            merge_parents = new List<string>(Regex.Split(match.Groups [2].Value, @" +"));
                            merge_ours = !match.Groups[1].Value.Equals("");
                        } else if (Regex.IsMatch(todoLine, @"^\s*$")) {
                        } else {
                            onSyntaxError(String.Format("Unrecognised todo line for mode {0}: {1}", mode, todoLine));
                        }
                        break;
                    case Mode.COMMENT:
                        if (Regex.IsMatch(todoLine, @"^\.$")) {
                            mode = Mode.COMMAND;
                            Todo.Add(Types.Step.NewComment(comment));
                        } else {
                            comment = comment + todoLine + "\n";
                        }
                        break;
                    case Mode.MERGE_COMMENT:
                        if (Regex.IsMatch(todoLine, @"^\.$")) {
                            mode = Mode.COMMAND;
                            Todo.Add(Types.Step.NewMerge(merge_parents, comment, merge_ours));
                        } else {
                            comment = comment + todoLine + "\n";
                        }
                        break;
                }
            }
            if (mode != Mode.COMMAND)
                onSyntaxError("Unterminated comment");
            return Tuple.Create(Todo, unknownCommits);
        }

        static string RebaseDir()
        {
            return Path.Combine(Environment.GitDir, "rebase2");
        }

        static string RebasePath(string subpath)
        {
            return Path.Combine(RebaseDir(), subpath);
        }
    }
}
