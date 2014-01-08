using System;
using System.Linq;
using System.Collections.Generic;

namespace rebase2 {
public class Utils {
    public static T Retry<T, Err>(Func<T> func) where Err: Exception
    {
        while (true) {
            try {
                return func();
            } catch (Err err) {
                Console.Error.WriteLine("Error: {0}\nRetry (Y/n)?", err.Message);
                string input = Console.In.ReadLine();
                if (!(input != null && (input.StartsWith("y") || input.StartsWith("Y")))) {
                    throw;
                }
            }
        }
    }

    private class CommitX {
        public int children_count;
        public ICollection<string> children;
        public Tuple<int, HashSet<string>> cost;
        public String sequence_child;
    }

    public static IEnumerable<string> FindSequence(IDictionary<string, Types.Commit> commits, string from, string to, IEnumerable<string> throughList)
    {
        // initialize
        var through = new HashSet<string>(throughList);
        var commitsX = new Dictionary<string, CommitX>();
        foreach (var commitItem in commits)
            commitsX.Add(commitItem.Key, new CommitX() { children_count = 0, children = new List<string>() });
        commitsX.Add(from, new CommitX() { children_count = 0, children = new List<string>() });
        commitsX.Add(to, new CommitX() { children_count = 0, children = new List<string>(), cost = Tuple.Create(0, new HashSet<string>()) });
        // fill children_count
        foreach (var commitItem in commits)
            foreach (string parent in commitItem.Value.parents)
                if (commitsX.ContainsKey(parent))
                    commitsX[parent].children_count++;
        // mark commits
        var edge = new HashSet<string>() { to };
        while (edge.Count > 0) {
            var next_edge = new HashSet<string>();
            foreach (var v in edge) {
                foreach (var parent in commits[v].parents) {
                    commitsX[parent].children.Add(v);
                    if (commitsX[parent].children.Count == commitsX[v].children_count) {
                        // all children are filled, find the best one
                        var optimum = FindMinimumCost(from c in commitsX[parent].children select Tuple.Create(commitsX[c].cost, c));
                        commitsX[parent].cost = Tuple.Create(optimum.Item1.Item1 + 1, optimum.Item1.Item2);
                        if (through.Contains(parent)) {
                            var newThroughs = new HashSet<string>(commitsX[parent].cost.Item2);
                            newThroughs.Add(parent);
                            commitsX[parent].cost = Tuple.Create(commitsX[parent].cost.Item1, newThroughs);
                        }
                        commitsX[parent].sequence_child = optimum.Item2;
                        next_edge.Add(parent);
                    }
                }
            }
            edge = next_edge;
        }
        var Result = new List<string>();
        var next = from;
        while (!next.Equals(to)) {
            next = commitsX[next].sequence_child;
            if (commitsX[next].children_count > 1)
                throw new Exception(string.Format("inner merges not supported yet (found in commit {0})", next));
            Result.Add(next);
        }
        return Result;
    }

    public static Tuple<Tuple<int, HashSet<string>>, T2> FindMinimumCost<T2>(IEnumerable<Tuple<Tuple<int, HashSet<string>>, T2>> Items)
    {
        bool HasOne = false;
        Tuple<Tuple<int, HashSet<string>>, T2> Result = null;
        foreach (var Item in Items) {
            if (!HasOne) {
                Result = Item;
                HasOne = true;
            } else {
                bool IsBetter = false;
                var ResultCost = Result.Item1;
                var ItemCost = Item.Item1;
                if (ResultCost.Item2.IsSubsetOf(ItemCost.Item2)) {
                    if (ItemCost.Item2.IsSubsetOf(ResultCost.Item2))
                        IsBetter = ResultCost.Item1 > ItemCost.Item1;
                    else
                        IsBetter = true;
                } else {
                    if (ItemCost.Item2.IsSubsetOf(ResultCost.Item2))
                        IsBetter = false;
                    else
                        throw new Exception(string.Format("Non-subsequent throughs not supported (met {0} vs {1})", ItemCost.Item2, ResultCost.Item2));
                }
                if (IsBetter) {
                    Result = Item;
                    HasOne = true;
                }
            }
        }
        if (!HasOne)
            throw new Exception("Empty sequence not supported");
        return Result;
    }

    public static void Assert<T>(bool Condition, T Message)
    {
        if (!Condition)
            throw new Exception(String.Format("Assertion failed: {0}", Message));
    }
}
}

