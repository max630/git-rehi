using System;
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

    private struct CommitX {
        int children_count;
        ICollection<string> children;
        Tuple<int, ISet<string>> cost;
    }

    public static IEnumerable<string> FindSequence(Types.Commits commits, string from, string to, IEnumerable<string> throughList)
    {
        // initialize
        var through = new HashSet<string>(throughList);
        var commitsX = new HashMap<string, CommitX>();
        foreach (var commitItem in commits)
            commitsX.Add(commitItem.Item1, new CommitX() { children_count = 0, children = new ArrayList<string>() });
        commitsX.Add(from, new CommitX() { children_count = 0, children = new ArrayList<string>() });
        commitsX.Add(to, new CommitX() { children_count = 0, children = new ArrayList<string>(), Tuple.Create(0, new HashSet<string>()) });
        // fill children_count
        foreach (var commitItem in commits)
            foreach (string parent in commitItem.Item2.parents)
                if (commitsX.Contains(parent))
                    commitsX.Get(parent).children_count++;
        // mark commits
        var edge = new HashSet<string>(to);
        while (edge.Count > 0) {
            var next_edge = new HashSet<string>();
            foreach (var v in edge) {
                var vertex_cost = commitsX.Item[v].cost;
                foreach (var parent in commits.Item[v].parents) {
                    commitsX.Item[parent].children.Add(v);
                    if (commitsX.Item[parent].children.Count == commitsX.Item(v).children_count) {
                        // all children are filled, find the best one
                        var optimum = FindMinimumCost(from c in commitsX.Item[parent].children select Tuple.Create(commitsX.Item[c].cost, c));
                        commitsX.Item[parent].cost = Tuple.Create(optimum.Item1.Item1 + 1, optimum.Item1.Item2);
                        if (through.Contains(parent))
                            commitsX.Item[parent].cost.Item2 = commitsX.Item[parent].cost.Item2.Union(new {parent});
                        commitsX.Item[parent].sequence_child = optimum.Item2;
                        next_edge.Add(parent);
                    }
                }
            }
            edge = next_edge;
        }
        var Result = new List<string>();
        var next = from;
        while (!next.Equals(to)) {
            next = commitsX.Item[next].sequence_child;
            if (commitsX.Item[next].children_count > 1)
                throw new Exception(string.Format("inner merges not supported yet (found in commit {0})", next));
            Result.Add(next);
        }
        return Result;
    }

    public static Tuple<Tuple<int, ISet<string>>, T2> FindMinimumCost<T2>(IEnumerable<Tuple<Tuple<int, ISet<string>>, T2>> Items)
    {
        bool HasOne = false;
        Tuple<Tuple<int, ISet<string>>, T2> Result = null;
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

    public static T getSingle<T>(IEnumerable<T> Items)
    {
        bool HasOne = false;
        T Result = default(T);
        foreach (var Item in Items) {
            if (HasOne)
                throw new Exception(string.Format("Single element collection is required: {0}", Items));
            HasOne = true;
            Result = Item;
        }
        if (!HasOne)
            throw new Exception(string.Format("Single element collection is required: {0}", Items));
        return Result;
    }


}
}

