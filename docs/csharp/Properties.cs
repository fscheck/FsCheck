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

        public static void Samples<T>([CallerFilePath] string file = "") {

            Prop.ForAll<T[]>(ts => RevRevIsOriginal(ts)).QuickCheck();

            Prop.ForAll<int[]>(ts => RevRevIsOriginalInt(ts)).QuickCheck();

            //[insertKeepsOrder]
            Prop.ForAll<int, int[]>((x, xs) => xs.Insert(x).IsOrdered())
                .When((x, xs) => xs.IsOrdered())
                .QuickCheck();
            //[/insertKeepsOrder]

            //[lazy]
            Prop.ForAll<int>(a => 1 / a == 1 / a)
                .When(a => a != 0)
                .QuickCheck();
            //[/lazy]

            //[insertWithArb]
            var orderedList = Arb.From<int[]>()
                .MapFilter(xs => xs.OrderBy(i => i).ToArray(), xs => xs.IsOrdered());
            Prop.ForAll<int>(x =>
                    Prop.ForAll(orderedList, xs => xs.Insert(x).IsOrdered())))
                .QuickCheck();
            //[/insertWithArb]

        }

            
    }
}
