using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using FsCheck;

namespace FsCheck.CSharpExamples
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
            T previous = default(T);
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

    class Program
    {
        static void Main(string[] args)
        {
            //A Simple example
            Spec.ForAny<int[]>(xs => xs.Reverse().Reverse().SequenceEqual( xs ))
                .QuickCheck();

            Spec.ForAny<int[]>(xs => xs.Reverse().SequenceEqual(xs))
                .QuickCheck();

            //Grouping properties : TODO


            //--------Properties--------------
            Spec.ForAny<double[]>(xs => xs.Reverse().Reverse().SequenceEqual(xs))
                .QuickCheck();

            //conditional properties
            Spec.ForAny<int, int[]>((x, xs) => xs.Insert(x).IsOrdered())
                .When((x, xs) => xs.IsOrdered())
                .QuickCheck();

            Spec.ForAny<int>(a => 1 / a == 1 / a)
                .When(a => a != 0)
                .QuickCheck();

            //counting trivial cases
            Spec.ForAny<int, int[]>((x, xs) => xs.Insert(x).IsOrdered())
                .When((x, xs) => xs.IsOrdered())
                .Classify( (x,xs) => xs.Count() == 0, "trivial")
                .QuickCheck();

            //classifying test values
            Spec.ForAny<int, int[]>((x, xs) => xs.Insert(x).IsOrdered())
                .When((x, xs) => xs.IsOrdered())
                .Classify((x, xs) => new int[] { x }.Concat(xs).IsOrdered(), "at-head")
                .Classify((x, xs) => xs.Concat(new int[] { x }).IsOrdered(), "at-tail")
                .QuickCheck();

            //collecting data values
            Spec.ForAny<int, int[]>((x, xs) => xs.Insert(x).IsOrdered())
                .When((x, xs) => xs.IsOrdered())
                .Collect((x, xs) => "length " + xs.Count().ToString())
                .QuickCheck();

            //combining observations
            Spec.ForAny<int, int[]>((x, xs) => xs.Insert(x).IsOrdered())
                .When((x, xs) => xs.IsOrdered())
                .Classify((x, xs) => new int[] { x }.Concat(xs).IsOrdered(), "at-head")
                .Classify((x, xs) => xs.Concat(new int[] { x }).IsOrdered(), "at-tail")
                .Collect((x, xs) => "length " + xs.Count().ToString())
                .QuickCheck();

            //---labelling sub properties-----
            //hmm. Cannot express result = m + n once this way.
            Spec.ForAny<int, int>((m, n) => m + n >= m).Label("result > #1") //maybe add overload with label to ForAny?
                .And((m, n) => m + n >= n, "result > #2")
                .And((m, n) => m + n < m + n, "result not sum")
                .QuickCheck();

            //misc tests
            //Spec.For(Any.OfType<char>(), c => c.Equals('a'))
            //     .When(c => c == 'a')
            //     .AndFor(Any.OfType<int>(), i => i> 10)
            //     .Classify((c,i) => i >5, "bigger")
            //     .QuickCheck();

            var gen = from x in Any.OfType<int>()
                      from y in Any.IntBetween(5, 10)
                      where x > 5
                      select new { Fst = x, Snd = y };

            
            Console.ReadKey();
        }

        
    }
}
