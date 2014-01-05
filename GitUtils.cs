using System;
using System.Collections.Generic;
using System.IO;

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

        public static IEnumerable<string> resolveHashes(IEnumerable<string> Refs)
        {
            foreach (var Ref in Refs) {
            }
        }
    }
}
