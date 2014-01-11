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
                    using (var TmpStream = new StreamWriter(File.Open(TmpPath, FileMode.CreateNew))) {
                    }
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

        class MonoFuncs
        {
            static MonoFuncs _instance = null;
            private MonoFuncs() { }

            public static MonoFuncs Instance {
                get {
                    if (_instance == null) {
                        var new_instance = new MonoFuncs();
                        // I don't like using Load with full name insted of LoadWIthPartialName, but otherwise error is not thrown and null is returned
                        // let's play their game
                        var mono = System.Reflection.Assembly.Load("Mono.Posix, Version=4.0.0.0, Culture=neutral, PublicKeyToken=0738eb9f132ed756");
                        var stdlib = mono.GetType("Mono.Unix.Native.Stdlib", true);
                        new_instance._system = stdlib.GetMethod("system");
                        Utils.Assert(new_instance._system != null, "method Mono.Unix.Native.Stdlib.system not found");
                        _instance = new_instance;
                    }
                    return _instance;
                }
            }

            System.Reflection.MethodInfo _system;

            public int system(string command)
            {
                return (Int32)(_system.Invoke(null, new object[] { command }));
            }
        }

        public static void Run(string command)
        {
            if (System.Environment.OSVersion.Platform == PlatformID.Unix) {
                var result = MonoFuncs.Instance.system(command);
                if (result != 0)
                    throw new Exception(String.Format("Command failed (exit code = {0}): {1}", result, command));
            } else {
                using (var p = new System.Diagnostics.Process()) {
                    throw new NotImplementedException("pick proper shell and other stuff");
                    #if false
                    p.StartInfo.FileName = "/bin/sh";
                    // same as g_escape_shell
                    var args = new System.Text.StringBuilder();
                    args.Append(@"-c '");
                    foreach (var c in command) {
                        if (c == '\'')
                            args.Append(@"'\''");
                        else
                            args.Append(c);
                    }
                    args.Append(@"'");
                    p.StartInfo.Arguments = args.ToString();
                    p.StartInfo.UseShellExecute = false;
                    p.Start();
                    p.WaitForExit();
                    if (p.ExitCode != 0)
                        throw new Exception(String.Format("Command failed (exit code = {0}): {1} {2}", p.ExitCode, command, args));
                    #endif
                }
            }
        }
    
        #if false
        public static void Run(string command, string aaa)
        {
            throw new NotImplementedException();
        }
        #endif

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

