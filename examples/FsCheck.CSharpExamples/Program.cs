using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
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
        
        static void Main(string[] args)
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
                    .Classify(xs.Count() == 0, "trivial"))
                .QuickCheck("InsertTrivial");

            //classifying test values
            Prop.ForAll<int, int[]>((x, xs) => 
                    xs.Insert(x).IsOrdered()
                    .When(xs.IsOrdered())
                    .Classify(new [] { x }.Concat(xs).IsOrdered(), "at-head")
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
                    .Classify(new [] { x }.Concat(xs).IsOrdered(), "at-head")
                    .Classify(xs.Concat(new [] { x }).IsOrdered(), "at-tail")
                    .Collect("length " + xs.Count().ToString()))
                .QuickCheck("InsertCombined");

            //---labelling sub properties-----
            Prop.ForAll<int, int>((n, m) => {
                var res = n * m;
                return  (new Func<bool>(() => res / m == n)).When(m != 0.0).Label("div1")
                   .And((new Func<bool>(() => res / n == m)).When(n != 0.0).Label("div2"))
                   .And((res > m).Label("lt1"))
                   .And((res > n).Label("lt2"))
                   .Label(string.Format("evidence = {0}", res));
            }).QuickCheck("Multiple labels");


            Prop.ForAll<int, int>((m, n) => {
                    var result = m + n;
                    return ( result >= m).Label("result > #1")
                       .And( result >= n).Label("result > #2")
                       .And( result < m + n).Label("result not sum");
            }).QuickCheck("ComplexProp");

            Prop.ForAll<int>(x => false.Label("Always false")
                            .And(Math.Abs(x) - x == 0))
                .QuickCheck("Label");


            //-------Test data generators-----------
            //can't be made generic, only in separate method?
            Func<int[],Gen<int>> chooseFromList = xs =>
                from i in Gen.Choose(0,xs.Length-1)
                select xs[i];
            
            var chooseBool = Gen.OneOf( Gen.Constant( true), Gen.Constant(false));

            var chooseBool2 = Gen.Frequency(
                new WeightAndValue<Gen<bool>>(2, Gen.Constant(true)),
                new WeightAndValue<Gen<bool>>(1, Gen.Constant(false)));

            //the size of test data : see matrix method

            //generating recursive data types: not so common in C#?

            // generating functions:
            Prop.ForAll((Func<int, int> f, Func<int, int> g, ICollection<int> a) => {
                            var l1 = a.Select(x => f(g(x)));
                            var l2 = a.Select(g).Select(f);
                            return l1.SequenceEqual(l2);
                        }).QuickCheck();

            //generators support select, selectmany and where
            var gen = from x in Arb.Generate<int>()
                      from int y in Gen.Choose(5, 10)
                      where x > 5
                      select new { Fst = x, Snd = y };

            //registering default arbitrary instances
            Arb.Register<MyArbitraries>();

            Prop.ForAll<long>(l => l + 1 > l)
                .QuickCheck();

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
                .QuickCheck();

            Arb.Generate<int>().Sample(10);
            Arb.Generate<int>().Sample(10, size:25);

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
            public static Arbitrary<long> Long() { return new ArbitraryLong(); }

            public static Arbitrary<IEnumerable<T>> Enumerable<T>() {
                return Arb.Default.Array<T>().Convert(x => (IEnumerable<T>)x, x => (T[])x);
            }

            public static Arbitrary<StringBuilder> StringBuilder() {
                return Arb.Generate<string>().Select(x => new StringBuilder(x)).ToArbitrary();
            }
        }
        
    }
}
