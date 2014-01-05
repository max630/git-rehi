using System;
using System.Collections.Generic;

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
                                string Next = p.StandardOutput.ReadLine();
                                if(Next == null)
                                    ProcessDispose();
                            }
                            return Opened;
                        },
                        cbCurrent = () => { return CurrentString; }
                    };
                }
            };
    }
    
    public static void verifyCmdArg(string arg)
    {
    }
}
}

