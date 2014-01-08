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
                    Tuple<List<Types.Step>, Types.Step, Types.Commits, string> Restored = restoreRebase();
                    if (Restored.Item2 != null) {
                        IOUtils.Run("git", "reset --hard HEAD");
                        File.Delete(Path.Combine(Environment.GitDir, "rebase2", "current"));
                    }
                    runRebase(Restored.Item1, Restored.Item3, Restored.Item4);
                    return;
                } else if (args[offset].Equals("--continue")) {
                    if (offset + 1 < args.Length)
                        throw new Exception(String.Format("Extra arguments after {0}: {1}", args[offset], String.Join(", ", Enumerable.Skip(args, offset + 1))));
                    Tuple<List<Types.Step>, Types.Step, Types.Commits, string> Restored = restoreRebase();
                    if (Restored.Item2 != null) {
                        runContinue(Restored.Item2, Restored.Item3);
                        File.Delete(Path.Combine(Environment.GitDir, "rebase2", "current"));
                    }
                    runRebase(Restored.Item1, Restored.Item3, Restored.Item4);
                    return;
                } else if (offset + 1 == args.Length) {
                    GitUtils.verifyClean();
                    var headRef = IOUtils.ReadPopen("git", "symbolic-ref -q HEAD");
                    Match match = null;
                    if ((match = Regex.Match(headRef, @"^/refs/heads/(.*)")).Success)
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
            Tuple<List<Types.Step>, Types.Commits, string, string> InitInfo = initRebase(real_dest, source_from, source_to, through);
            List<Types.Step> Todo = InitInfo.Item1;
            if (isInteractive) {
                bool isOk;
                Todo = new List<Types.Step>(editTodo(Todo, InitInfo.Item2, out isOk));
                if (!isOk) {
                    cleanupSave();
                    Console.WriteLine("Aborted");
                    return;
                }
            }
            if (Todo.Count > 0) {
                IOUtils.Run("git", String.Format("checkout --quiet --detach {0}", InitInfo.Item4));
                runRebase(Todo, InitInfo.Item2, InitInfo.Item3);
            } else {
                cleanupSave();
                Console.WriteLine("Nothing to do.");
            }
        }

        static Tuple<List<Types.Step>, Types.Step, Types.Commits, string> restoreRebase()
        {
            string targetRef = Enumerable.Single<string>(File.ReadLines(RebasePath("target_ref")));
            var commits = loadCommits();
            Tuple<List<Types.Step>, List<string>> TodoTuple = readTodo(RebasePath("target_ref"), commits);
            Types.Step current = File.Exists(RebasePath("current")) ? Enumerable.Single<Types.Step>(readTodo(RebasePath("current"), commits).Item1) : null;
            return Tuple.Create(TodoTuple.Item1, current, commits, targetRef);
        }

        static Tuple<List<Types.Step>, Types.Commits, string, string> initRebase(string dest, string source_from, string source_to, IEnumerable<string> through)
        {
            throw new NotImplementedException();
        }

        static void cleanupSave()
        {
            throw new NotImplementedException();
        }

        static void runContinue(Types.Step step, Types.Commits commits)
        {
            throw new NotImplementedException();
        }

        static void runRebase(IEnumerable<Types.Step> todo, Types.Commits commits, string target_ref)
        {
            throw new NotImplementedException();
        }

        static IEnumerable<Types.Step> editTodo(IEnumerable<Types.Step> oldTodo, Types.Commits commits, out bool isOk)
        {
            throw new NotImplementedException();
        }

        public static Types.Commits loadCommits()
        {
            throw new NotImplementedException();
        }

        public static void saveTodo(List<Types.Step> Todo, string todoFile, Types.Commits commits)
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

        static string RebasePath(string subpath)
        {
            return Path.Combine(Environment.GitDir, "rebase2", subpath);

        }
    }
}
