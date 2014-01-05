using System;
using System.IO;

namespace rebase2 {
    public class GitUtils {
        public static bool getNoUncommittedChanges()
        {
            var p = new System.Diagnostics.Process("git", "diff-index --quiet --ignore-submodules HEAD");
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
    }
}
