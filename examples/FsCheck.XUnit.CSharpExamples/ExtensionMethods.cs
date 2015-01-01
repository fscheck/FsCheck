using System;
using System.Collections.Generic;
using System.Linq;

namespace FsCheck.XUnit.CSharpExamples
{
    public static class ExtensionMethods
    {
        public static IEnumerable<TSource> BadReverse1<TSource>(this IEnumerable<TSource> source)
        {
            return source;
        }

        public static IEnumerable<TSource> BadReverse2<TSource>(this IEnumerable<TSource> source)
        {
            var badReverse2 = source as IList<TSource> ?? source.ToList();
            return badReverse2.Count() > 10 ? badReverse2 : badReverse2.Reverse();
        }

        public static IEnumerable<TSource> BadReverse3<TSource>(this IEnumerable<TSource> source) where TSource : IEquatable<TSource>
        {
            var list = source.ToList();
            var isOrdered = list.SequenceEqual(list.AsEnumerable().OrderBy(_ => _));
            return list.Count >= 3 && isOrdered ? list : list.AsEnumerable().Reverse();
        }
    }
}