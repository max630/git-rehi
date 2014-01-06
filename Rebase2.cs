using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Collections.Generic;

namespace rebase2 {
    public class Rebase2 {
        public static void saveTodo(List<Types.Step> Todo, string File, Types.Commits commits)
        {
            using (var Out = File.CreateText(File)) {
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
                            if (command.Contains('\n'))
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

        public static Tuple<List<Types.Step>, List<string>> readTodo(string Filename, Types.Commits commits, Func<string> onSyntaxError)
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
            Tuple.Create(Todo, unknownCommits);
        }
    }
}
