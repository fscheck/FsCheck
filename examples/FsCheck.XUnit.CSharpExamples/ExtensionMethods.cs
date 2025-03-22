using System;
using System.Collections.Generic;
using System.Linq;

namespace FsCheck.XUnit.CSharpExamples
{
    public static class ExtensionMethods
    {
        /// <summary> Never reverts the <paramref name="list"/> </summary>
        public static IEnumerable<TSource> BadReverse1<TSource>(this IEnumerable<TSource> list)
        {
            return list;
        }

        /// <summary> When the <paramref name="list"/> is longer than 10, doesn't revert it </summary>
        public static IEnumerable<TSource> BadReverse2<TSource>(this IEnumerable<TSource> list)
        {
            var badReverse2 = list as IList<TSource> ?? list.ToList();
            return badReverse2.Count > 10 ? badReverse2 : badReverse2.Reverse();
        }

        /// <summary> When the <paramref name="list"/> is accidentally sorted, doesn't revert it! </summary>
        public static IEnumerable<TSource> BadReverse3<TSource>(this IEnumerable<TSource> list) where TSource : IEquatable<TSource>
        {
            var copy = list.ToList();
            var isOrdered = copy.SequenceEqual(copy.AsEnumerable().OrderBy(_ => _)); //O(nLog(n)) instead of O(n)
            return copy.Count >= 3 && isOrdered ? copy : copy.AsEnumerable().Reverse();
        }
    }
}
