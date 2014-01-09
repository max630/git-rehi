using System;
using System.IO;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace rebase2 {
public class IOUtils {
    private class Enumerablie : IEnumerable<string> {
        public Func<IEnumerator<string>> cbEnumerator;

        IEnumerator<string> IEnumerable<string>.GetEnumerator()
        {
            return cbEnumerator();
        }

        public System.Collections.IEnumerator GetEnumerator()
        {
            return cbEnumerator();
        }
    }

    private class Enumerator : IEnumerator<string> {
        public Action cbDispose;
        public Func<string> cbCurrent;
        public Func<Boolean> cbNext;
        public Action cbReset;

        void IDisposable.Dispose()
        {
            cbDispose();
        }

        bool System.Collections.IEnumerator.MoveNext()
        {
            return cbNext();
        }

        void System.Collections.IEnumerator.Reset()
        {
            cbReset();
        }

        string IEnumerator<string>.Current { get { return cbCurrent(); } }

        object System.Collections.IEnumerator.Current { get { return cbCurrent(); } }
    }

    public static IEnumerable<string> EnumPopen(string Program, string Args)
    {
        return new Enumerablie() {
                cbEnumerator = () => {
                    var p = new System.Diagnostics.Process();
                    p.StartInfo.UseShellExecute = false;
                    p.StartInfo.FileName = Program;
                    p.StartInfo.Arguments = Args;
                    p.StartInfo.RedirectStandardOutput = true;
                    p.Start();
                    string CurrentString = null;
                    bool Opened = true;
                    Action ProcessDispose = () => {
                        if (Opened) {
                            p.Dispose();
                            Opened = false;
                        }
                    };
                    return new Enumerator() {
                        cbDispose = () => ProcessDispose(),
                        cbReset = () => { throw new NotImplementedException(); },
                        cbNext = () => {
                            if (Opened) {
                                CurrentString = p.StandardOutput.ReadLine();
                                if(CurrentString == null)
                                    ProcessDispose();
                            }
                            return Opened;
                        },
                        cbCurrent = () => { return CurrentString; }
                    };
                }
            };
    }

    public static string MakeTempFile(string suffix)
    {
        for (int i = 0; true; ++i) {
            string TmpPath = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName() + suffix);
            try {
                using (var TmpStream = new StreamWriter(File.Open(TmpPath, FileMode.CreateNew))) { }
                return TmpPath;
            } catch (IOException ex) {
                if (i >= 5)
                    throw new Exception("Cannot create temporary file", ex);
            }
        }
    }
    
    public static void verify_cmdarg(string arg)
    {
        if (Regex.IsMatch(arg, @"[""'\\\(\)#]|[\x00- ]"))
            throw new Exception(String.Format("Invalid cmdarg: `{0}'", arg));
    }

    public static void Run(string command, string args)
    {
        using (var p = new System.Diagnostics.Process()) {
            p.StartInfo.FileName = command;
            p.StartInfo.Arguments = args;
            p.StartInfo.UseShellExecute = false;
            p.Start();
            p.WaitForExit();
            if (p.ExitCode != 0)
                throw new Exception(String.Format("Command failed (exit code = {0}): {1} {2}", p.ExitCode, command, args));
        }
    }

    public static string ReadPopen(string command, string args)
    {
        using (var p = new System.Diagnostics.Process()) {
            p.StartInfo.FileName = command;
            p.StartInfo.Arguments = args;
            p.StartInfo.UseShellExecute = false;
            p.StartInfo.RedirectStandardOutput = true;
            p.Start();
            var Output = p.StandardOutput.ReadToEnd();
            p.WaitForExit();
            if (p.ExitCode != 0)
                throw new Exception(String.Format("Command failed (exit code = {0}): {1} {2}", p.ExitCode, command, args));
            return Output;
        }
    }
}
}

