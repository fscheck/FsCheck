using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace FsCheck.MsTest.Examples.ClassesToTest
{
    public static class Extensions
    {
        public static IEnumerable<int> Insert(this IEnumerable<int> cs, int x)
        {
            var result = new List<int>(cs);
            foreach (var c in cs)
            {
                if (x <= c)
                {
                    result.Insert(result.IndexOf(c), x);
                    return result;
                }
            }
            result.Add(x);
            return result;
        }

        public static bool IsOrdered<T>(this IEnumerable<T> source)
        {
            //by Jon Skeet
            //I was too lazy to write it myself, and wondered whether a prettier 
            //solution might exist in C# than the one I had in mind.
            //Here's your answer...
            var comparer = Comparer<T>.Default;
            T previous = default;
            bool first = true;

            foreach (T element in source)
            {
                if (!first && comparer.Compare(previous, element) > 0)
                {
                    return false;
                }
                first = false;
                previous = element;
            }
            return true;
        }
    }

}
