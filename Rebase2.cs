using System;
using System.IO;
using System.Text.RegularExpressions;
using System.Collections.Generic;

namespace rebase2 {
public class Rebase2 {
    enum Mode {
        COMMAND,
        COMMENT,
        MERGE_COMMENT,
    }

    public static void readTodo(string Filename, Types.Commits commits, Func<string> onSyntaxError)
    {
        var Todo = new List<Types.Step>();
        var unknownCommits = new List<string>();
        Mode mode = Mode.COMMAND;
        string comment = "";
        List<string> merge_parents = null;
        foreach (var todoLine in File.ReadLines(Filename)) {
            Match match;
            if (Regex.IsMatch(todoLine, @"^\s*#"))
                continue;
            switch (mode) {
                case Mode.COMMAND:
                    if ((match = Regex.Match(todoLine, @"^(f|fixup) ([0-9a-f]+)( (.*))?$")).Success) {
                        Todo.Add(Types.Step.NewFixup(match.Groups[2].Value));
                        if (!commits.byAHash.ContainsKey(match.Groups[2].Value))
                            unknownCommits.Add(match.Groups[2].Value);
                    } else if ((match = Regex.Match(todoLine, @"^(p|pick) ([0-9a-f]+)( (.*))?$")).Success) {
                        Todo.Add(Types.Step.NewPick(match.Groups[2].Value));
                        if (!commits.byAHash.ContainsKey(match.Groups[2].Value))
                            unknownCommits.Add(match.Groups[2].Value);
                    } else if ((match = Regex.Match(todoLine, @"^(e|edit) ([0-9a-f]+)( (.*))?$")).Success) {
                        Todo.Add(Types.Step.NewEdit(match.Groups[2].Value));
                        if (!commits.byAHash.ContainsKey(match.Groups[2].Value))
                            unknownCommits.Add(match.Groups[2].Value);
                    } else if ((match = Regex.Match(todoLine, @"^(x|exec) (.+)$")).Success) {
                        Todo.Add(Types.Step.NewExec(match.Groups[2].Value));
                    } else if ((match = Regex.Match(todoLine, @"^comment\b$")).Success) {
                        mode = Mode.COMMENT;
                        comment = "";
                    } else if ((match = Regex.Match(todoLine, @"^merge( --ours)? (.+[^\n\r ])$")).Success) {
                        mode = Mode.MERGE_COMMENT;
                        comment = "";
                        merge_parents = new List<string>(Regex.Split(match.Groups[2].Value, @" +"));
                    }
                    break;
                case Mode.COMMENT:
                    break;
                case Mode.MERGE_COMMENT:
                    break;
            }
        }
    }
}
}
