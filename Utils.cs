using System;
using System.Collections.Generic;

namespace rebase2
{
    public class Utils
    {
        public static T Retry<T, Err>(Func<T> func) where Err: Exception
        {
            while(true) {
                try {
                    return func();
                } catch (Err err) {
                    Console.Error.WriteLine("Error: {0}\nRetry (Y/n)?", err.Message);
                    string input = Console.In.ReadLine();
                    if(!(input != null && (input.StartsWith("y") || input.StartsWith("Y")))) {
                        throw;
                    }
                }
            }
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
            bool HasOne;
            T Result = null;
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

