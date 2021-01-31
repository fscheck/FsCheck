using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FsCheck;
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

	///<returns> true iff source is strictly decreasing </returns>
        public static bool IsOrdered<T>(this IEnumerable<T> source)
        {
            var comparer = Comparer<T>.Default;
            return source.Zip(source.Skip(1), (a,b) => comparer.Compare(a,b) > 0)
		    .Aggregate((a,b) => a && b);
        }
    }

    class Program
    {
        class Eq : IEqualityComparer<double> {
            public bool Equals(double x, double y) {
                return x == y;
            }

            public int GetHashCode(double obj) {
                throw new NotImplementedException();
            }
        }

        static void Main()
        {
            Prop.ForAll<int>((i => Task.FromResult(i < 1000)))
                .QuickCheck("Task");

            //A simple example

            Prop.ForAll<double[]>(xs => xs.Reverse().Reverse().SequenceEqual(xs, new Eq()))
                .QuickCheck("RevRev");

            Prop.ForAll<int[]>(xs => xs.Reverse().SequenceEqual(xs))
                .QuickCheck("RevId");

            //Grouping properties : not yet implemented


            //--------Properties--------------
            //Default Comparison compares NaN properly
            Prop.ForAll<double[]>(xs => xs.Reverse().Reverse().SequenceEqual(xs))
                .QuickCheck("RevRevFloat");

            //conditional properties
            Prop.ForAll<int, int[]>((x, xs) => xs.Insert(x).IsOrdered().When(xs.IsOrdered()))
                .QuickCheck("Insert");

            Prop.ForAll<int>(a => Prop.When(a != 0, () => 1 / a == 1 / a))
                .QuickCheck("DivByZero");

            Prop.ForAll<short[]>(xs => Prop.When(condition: xs.Length > 0, assertion: () => xs.Length > 0))
                .QuickCheck("When non-empty array ==> Assert non-empty array");

            //counting trivial cases
            Prop.ForAll<int, int[]>((x, xs) =>
                    xs.Insert(x).IsOrdered()
                    .When(xs.IsOrdered())
                    .Classify(xs.Length == 0, "trivial"))
                .QuickCheck("InsertTrivial");

            //classifying test values
            Prop.ForAll<int, int[]>((x, xs) =>
                    xs.Insert(x).IsOrdered()
                    .When(xs.IsOrdered())
                    .Classify(new[] { x }.Concat(xs).IsOrdered(), "at-head")
                    .Classify(xs.Concat(new int[] { x }).IsOrdered(), "at-tail"))
                .QuickCheck("InsertClassify");

            //collecting data values
            Prop.ForAll<int, int[]>((x, xs) =>
                    xs.Insert(x).IsOrdered()
                    .When(xs.IsOrdered())
                    .Collect("length " + xs.Count().ToString()))
                .QuickCheck("InsertCollect");

            //combining observations
            Prop.ForAll<int, int[]>((x, xs) =>
                    xs.Insert(x).IsOrdered()
                    .When(xs.IsOrdered())
                    .Classify(new[] { x }.Concat(xs).IsOrdered(), "at-head")
                    .Classify(xs.Concat(new[] { x }).IsOrdered(), "at-tail")
                    .Collect("length " + xs.Count().ToString()))
                .QuickCheck("InsertCombined");

            //---labelling sub properties-----
            Prop.ForAll<int, int>((n, m) => {
                var res = n * m;
                return (new Func<bool>(() => res / m == n)).When(m != 0.0).Label("div1")
                   .And((new Func<bool>(() => res / n == m)).When(n != 0.0).Label("div2"))
                   .And((res > m).Label("lt1"))
                   .And((res > n).Label("lt2"))
                   .Label(string.Format("evidence = {0}", res));
            }).QuickCheck("Multiple labels");


            Prop.ForAll<int, int>((m, n) => {
                var result = m + n;
                return (result >= m).Label("result > #1")
                   .And(result >= n).Label("result > #2")
                   .And(result < m + n).Label("result not sum");
            }).QuickCheck("ComplexProp");

            Prop.ForAll<int>(x => false.Label("Always false")
                            .And(Math.Abs(x) - x == 0))
                .QuickCheck("Label");


            //-------Test data generators-----------

            var chooseBool = Gen.OneOf(Gen.Constant(true), Gen.Constant(false));

            var chooseBool2 = Gen.Frequency(
                (2, Gen.Constant(true)),
                (1, Gen.Constant(false)));

            //the size of test data : see matrix method

            //generating recursive data types: not so common in C#?

            // generating functions:
            Prop.ForAll((Func<int, int> f, Func<int, int> g, ICollection<int> a) => {
                var l1 = a.Select(x => f(g(x)));
                var l2 = a.Select(g).Select(f);
                return l1.SequenceEqual(l2);
            }).QuickCheck();

            //generators support select, selectmany and where
            var gen = from x in ArbMap.Default.GeneratorFor<int>()
                      from int y in Gen.Choose(5, 10)
                      where x > 5
                      select new { Fst = x, Snd = y };

            //overriding arbitrary instances

            Prop.ForAll<long>(l => l + 1 > l)
                .Check(Config.Default.WithArbitrary(new[] { typeof(MyArbitraries) }));

            Prop.ForAll<string>(s => true)
                .Check(Config.Default.WithName("Configuration Demo").WithMaxTest(500));

            // discard

            Prop.ForAll<int>(s => s >= 4 || Prop.Discard<bool>()).QuickCheck();

            Console.WriteLine();

            // replay
            Prop.ForAll<string>(s => s == null)
                .Label("Replay")
                .Check(Config.Default.WithMaxTest(1).WithReplay(1UL, 1UL));

            Console.WriteLine();

            Prop.ForAll((IEnumerable<int> a, IEnumerable<int> b) => 
                            a.Except(b).Count() <= a.Count())
                .Check(Config.Default.WithArbitrary(new[] { typeof(MyArbitraries) }));

            ArbMap.Default.GeneratorFor<int>().Sample(10);
            ArbMap.Default.GeneratorFor<int>().Sample(10, size:25);

			Console.WriteLine("Press any key...");
            Console.ReadKey();
        }

        public static Gen<T> Matrix<T>(Gen<T> gen)
        {
            return Gen.Sized(s => gen.Resize(Convert.ToInt32(Math.Sqrt(s))));
        }

        public class ArbitraryLong : Arbitrary<long>
        {
            public override Gen<long> Generator
            {
	            get {
                    return Gen.Sized(s => Gen.Choose(-s, s))
                        .Select(i => Convert.ToInt64(i));
                }
            }
        }


        public class MyArbitraries
        {
            public static Arbitrary<long> Long { get; } = new ArbitraryLong();

            public static Arbitrary<IEnumerable<T>> Enumerable<T>() {
                return ArbMap.Default.ArbFor<T[]>().Convert(x => (IEnumerable<T>)x, x => (T[])x);
            }

            public static Arbitrary<StringBuilder> StringBuilder() {
                return ArbMap.Default.GeneratorFor<string>().Select(x => new StringBuilder(x)).ToArbitrary();
            }
        }
        
    }
}
