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
        Mode mode = Mode::COMMAND;
        foreach (var todoLine in File.ReadLines(Filename)) {
            if (!Regex.IsMatch(todoLine, @"^") WRITE_HERE
            switch (mode) {
                case Mode::COMMAND:
                    if
                    break;
                case Mode::COMMENT:
                    break;
                case Mode::MERGE_COMMENT:
                    break;
            }
        }
    }
}
}
