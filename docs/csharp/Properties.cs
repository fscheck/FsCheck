using FsCheck;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;

namespace CSharp.DocSnippets
{
    public static class Extensions {
        public static IEnumerable<int> Insert(this IEnumerable<int> cs, int x) {
            var result = new List<int>(cs);
            foreach (var c in cs) {
                if (x <= c) {
                    result.Insert(result.IndexOf(c), x);
                    return result;
                }
            }
            result.Add(x);
            return result;
        }

        public static bool IsOrdered<T>(this IEnumerable<T> source) {
            //by Jon Skeet
            //I was too lazy to write it myself, and wondered whether a prettier 
            //solution might exist in C# than the one I had in mind.
            //Here's your answer...
            var comparer = Comparer<T>.Default;
            T previous = default(T);
            bool first = true;

            foreach (T element in source) {
                if (!first && comparer.Compare(previous, element) > 0) {
                    return false;
                }
                first = false;
                previous = element;
            }
            return true;
        }
    }

    class Properties {
        //[revRevIsOrig]
        public static bool RevRevIsOriginal<T>(T[] ts) {
            return ts.Reverse().Reverse().SequenceEqual(ts);
        }
        //[/revRevIsOrig]

        //[revRevIsOrigInt]
        public static bool RevRevIsOriginalInt(int[] ts) {
            return ts.Reverse().Reverse().SequenceEqual(ts);
        }
        //[/revRevIsOrigInt]

        public static void Samples([CallerFilePath] string file = "") {

            //[insertKeepsOrder]
            Prop.ForAll<int, int[]>((x, xs) => xs.Insert(x).IsOrdered().When(xs.IsOrdered()))
                .QuickCheck();
            //[/insertKeepsOrder]

            //[lazy]
            Prop.ForAll<int>(a => new Func<bool>(() => 1 / a == 1 / a).When(a != 0))
                .QuickCheck();
            //[/lazy]

            //[insertWithArb]
            var orderedList = Arb.From<int[]>()
                                 .MapFilter(xs => xs.OrderBy(i => i).ToArray(), xs => xs.IsOrdered());

            Prop.ForAll<int>(x => Prop.ForAll(orderedList, xs => xs.Insert(x).IsOrdered()))
                .QuickCheck();
            //[/insertWithArb]
            
            //[insertTrivial]
            Prop.ForAll<int, int[]>((x, xs) =>
                        xs.Insert(x).IsOrdered()
                        .When(xs.IsOrdered())
                        .Classify(xs.Count() == 0, "trivial"))
                .QuickCheck();
            //[/insertTrivial]

            //[insertClassify]
            Prop.ForAll<int, int[]>((x, xs) =>
                    xs.Insert(x).IsOrdered()
                    .When(xs.IsOrdered())
                    .Classify(new[] { x }.Concat(xs).IsOrdered(), "at-head")
                    .Classify(xs.Concat(new int[] { x }).IsOrdered(), "at-tail"))
                .QuickCheck();
            //[/insertClassify]

            //[insertCollect]
            Prop.ForAll<int, int[]>((x, xs) =>
                    xs.Insert(x).IsOrdered()
                    .When(xs.IsOrdered())
                    .Collect("length " + xs.Count().ToString()))
                .QuickCheck();
            //[/insertCollect]

            //[insertCombined]
            Prop.ForAll<int, int[]>((x, xs) =>
                    xs.Insert(x).IsOrdered()
                    .When(xs.IsOrdered())
                    .Classify(new [] { x }.Concat(xs).IsOrdered(), "at-head")
                    .Classify(xs.Concat(new int[] { x }).IsOrdered(), "at-tail")
                    .Collect("length " + xs.Count().ToString()))
                .QuickCheck();
            //[/insertCombined]

        }

            
    }
}
