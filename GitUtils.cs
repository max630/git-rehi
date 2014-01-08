using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace rebase2 {
public class GitUtils {
    public static bool getNoUncommittedChanges()
    {
        var p = new System.Diagnostics.Process();
        p.StartInfo.FileName = "git";
        p.StartInfo.Arguments = "diff-index --quiet --ignore-submodules HEAD";
        p.Start();
        p.WaitForExit();
        return p.ExitCode == 0;
    }

    public static void verifyClean()
    {
        if (!getNoUncommittedChanges())
            throw new Exception("Not clean working directory");
        if (File.Exists(Path.Combine(Environment.GitDir, "rebase-apply")))
            throw new Exception("git-am or rebase in progress");
        if (File.Exists(Path.Combine(Environment.GitDir, "rebase-apply")))
            throw new Exception("rebase in progress");
    }

    public static IEnumerable<string> resolveHashes(IEnumerable<string> refs)
    {
        var refsList = new List<string>(refs);
        foreach (var rf in refsList)
            IOUtils.verifyCmdArg(rf);
        var Result = new List<string>(IOUtils.EnumPopen("git", String.Format("rev-parse {0}", String.Join(" ", refsList))));
        if (Result.Count != refsList.Count)
            throw new Exception("Hash number does not match");
        return Result;
    }

    public static string mergeBase(string ref1, string ref2)
    {
        IOUtils.verifyCmdArg(ref1);
        IOUtils.verifyCmdArg(ref2);
        return Enumerable.Single<string>(IOUtils.EnumPopen("git", String.Format("merge-base -a {0} {1}", ref1, ref2)));
    }
}
}
