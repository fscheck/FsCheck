using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using FsCheck.Fluent;

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
            //A simple example
            Spec.ForAny<int[]>(xs => xs.Reverse().Reverse().SequenceEqual( xs ))
                .QuickCheck("RevRev");

            Spec.ForAny<int[]>(xs => xs.Reverse().SequenceEqual(xs))
                .QuickCheck("RevId");

            //Grouping properties : not yet implemented


            //--------Properties--------------
            Spec.ForAny<double[]>(xs => xs.Reverse().Reverse().SequenceEqual(xs))
                .QuickCheck("RevRevFloat");

            //conditional properties
            Spec.ForAny<int, int[]>((x, xs) => xs.Insert(x).IsOrdered())
                .When((x, xs) => xs.IsOrdered())
                .QuickCheck("Insert");

            Spec.ForAny<int>(a => 1 / a == 1 / a)
                .When(a => a != 0)
                .QuickCheck("DivByZero");

            //counting trivial cases
            Spec.ForAny<int, int[]>((x, xs) => xs.Insert(x).IsOrdered())
                .When((x, xs) => xs.IsOrdered())
                .Classify( (x,xs) => xs.Count() == 0, "trivial")
                .QuickCheck("InsertTrivial");

            //classifying test values
            Spec.ForAny<int, int[]>((x, xs) => xs.Insert(x).IsOrdered())
                .When((x, xs) => xs.IsOrdered())
                .Classify((x, xs) => new int[] { x }.Concat(xs).IsOrdered(), "at-head")
                .Classify((x, xs) => xs.Concat(new int[] { x }).IsOrdered(), "at-tail")
                .QuickCheck("InsertClassify");

            //collecting data values
            Spec.ForAny<int, int[]>((x, xs) => xs.Insert(x).IsOrdered())
                .When((x, xs) => xs.IsOrdered())
                .Collect((x, xs) => "length " + xs.Count().ToString())
                .QuickCheck("InsertCollect");

            //combining observations
            Spec.ForAny<int, int[]>((x, xs) => xs.Insert(x).IsOrdered())
                .When((x, xs) => xs.IsOrdered())
                .Classify((x, xs) => new int[] { x }.Concat(xs).IsOrdered(), "at-head")
                .Classify((x, xs) => xs.Concat(new int[] { x }).IsOrdered(), "at-tail")
                .Collect((x, xs) => "length " + xs.Count().ToString())
                .QuickCheck("InsertCombined");

            //---labelling sub properties-----
            //hmm. Cannot express result = m + n once this way.
            Spec.ForAny<int, int>((m, n) => m + n >= m).Label("result > #1") //maybe add overload with label to ForAny?
                .And((m, n) => m + n >= n, "result > #2")
                .And((m, n) => m + n < m + n, "result not sum")
                .QuickCheck("ComplexProp");

            Spec.ForAny<int>(x => false).Label("Always false")
                .And(x => Math.Abs(x) - x == 0) //actually, And should start a new property, not just a new assertion...
                .QuickCheck("Label");

            //rest seem hard to express without real "And" and "Or" support

            //-------Test data generators-----------
            //can't be made generic, only in separate method?
            Func<int[],Gen<int>> chooseFromList = xs =>
                from i in Any.IntBetween(0,xs.Length-1)
                select xs[i];
            
            var chooseBool = Any.GeneratorIn( Any.Value( true), Any.Value(false));

            //no tuples in C# until BCL 4.0...can we do better now?
            var chooseBool2 = Any.WeighedValueIn(
                new WeightAndValue<bool>(2, true),
                new WeightAndValue<bool>(1, false));

            //the size of test data : see matrix method

            //generating recursive data types: not so common in C#?

            // generating functions:
            Spec.ForAny((Func<int, int> f, Func<int, int> g, ICollection<int> a) => {
                            var l1 = a.Select(x => f(g(x)));
                            var l2 = a.Select(g).Select(f);
                            return l1.SequenceEqual(l2);
                        }).QuickCheck();

            //generators support select, selectmany and where
            var gen = from x in Any.OfType<int>()
                      from y in Any.IntBetween(5, 10)
                      where x > 5
                      select new { Fst = x, Snd = y };

            //registering default arbitrary instances
            DefaultArbitraries.Add<MyArbitraries>();

            Spec.ForAny<long>(l => l + 1 > l)
                .QuickCheck();

            Spec.ForAny<string>(s => true)
                .Check(new Configuration { Name = "Configuration Demo", MaxNbOfTest = 500 });

            Spec.ForAny((IEnumerable<int> a, IEnumerable<int> b) => 
                            a.Except(b).Count() <= a.Count())
                .QuickCheck();

            Console.ReadKey();
        }

        public static FsCheck.Gen<T> Matrix<T>(Gen<T> gen)
        {
            return gen.Resize(s => Convert.ToInt32(Math.Sqrt(s)));
        }

        public class ArbitraryLong : Arbitrary<long>
        {
            public override Gen<long> Generator
            {
	            get { 
                    return Any.OfSize( s => Any.IntBetween(-s,s))
                        .Select( i => Convert.ToInt64(i)); 
                }
            }
        }


        public class MyArbitraries
        {
            public static Arbitrary<long> Long() { return new ArbitraryLong(); }

            public static Arbitrary<IEnumerable<T>> Enumerable<T>() {
                return Arb.Default.Array<T>().Convert(x => (IEnumerable<T>)x, x => (T[])x);
            }

            public static Arbitrary<StringBuilder> StringBuilder() {
                return Any.OfType<string>().Select(x => new StringBuilder(x)).ToArbitrary();
            }
        }
        
    }
}
