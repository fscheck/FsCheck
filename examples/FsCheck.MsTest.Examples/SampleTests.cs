using System;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using FsCheck.MsTest.Examples.ClassesToTest;

namespace FsCheck.MsTest.Examples
{
    [TestClass]
    public class SampleTests
    {
        [TestMethod]
        public void RevRev()
        {
            Prop.ForAll<int[]>(xs => xs.Reverse().Reverse().SequenceEqual(xs))
                .QuickCheckThrowOnFailure();
        }

        [TestMethod, ExpectedException(typeof(Exception))]
        public void RevId()
        {
            Prop.ForAll<int[]>(xs => xs.Reverse().SequenceEqual(xs))
                .VerboseCheckThrowOnFailure();
        }

        [TestMethod, ExpectedException(typeof(Exception))]
        public void Insert()
        {            
             Prop.ForAll<int, int[]>((x, xs) => xs.Insert(x).IsOrdered())
                .When((x, xs) => xs.IsOrdered())
                .QuickCheckThrowOnFailure();
        }

        [TestMethod]
        public void DivByZero()
        {
            Prop.ForAll<int>(a => 1 / a == 1 / a)
                .When(a => a != 0)
                .QuickCheckThrowOnFailure();
        }

        [TestMethod, ExpectedException(typeof(Exception))]
        public void InsertTrivial()
        {
            Prop.ForAll<int, int[]>((x, xs) => xs.Insert(x).IsOrdered())
                .When((x, xs) => xs.IsOrdered())
                .Classify( (x,xs) => xs.Count() == 0, "trivial")
                .QuickCheckThrowOnFailure();            
        }

        [TestMethod, ExpectedException(typeof(Exception))]
        public void InsertClassify()
        {
            Prop.ForAll<int, int[]>((x, xs) => xs.Insert(x).IsOrdered())
                .When((x, xs) => xs.IsOrdered())
                .Classify((x, xs) => new int[] { x }.Concat(xs).IsOrdered(), "at-head")
                .Classify((x, xs) => xs.Concat(new int[] { x }).IsOrdered(), "at-tail")
                .QuickCheckThrowOnFailure();            
        }

        [TestMethod, ExpectedException(typeof(Exception))]
        public void InsertCollect()
        {
            Prop.ForAll<int, int[]>((x, xs) => xs.Insert(x).IsOrdered())
                .When((x, xs) => xs.IsOrdered())
                .Collect((x, xs) => "length " + xs.Count().ToString())
                .QuickCheckThrowOnFailure();
        }

        [TestMethod, ExpectedException(typeof(Exception))]
        public void InsertCombined()
        {            
            Prop.ForAll<int, int[]>((x, xs) => xs.Insert(x).IsOrdered())
                .When((x, xs) => xs.IsOrdered())
                .Classify((x, xs) => new int[] { x }.Concat(xs).IsOrdered(), "at-head")
                .Classify((x, xs) => xs.Concat(new int[] { x }).IsOrdered(), "at-tail")
                .Collect((x, xs) => "length " + xs.Count().ToString())
                .QuickCheckThrowOnFailure();
        }

        [TestMethod, ExpectedException(typeof(Exception))]
        public void ComplexProp()
        {
            Prop.ForAll<int, int>((m, n) => m + n >= m).Label("result > #1")
                .And((m, n) => m + n >= n, "result > #2")
                .And((m, n) => m + n < m + n, "result not sum")
                .QuickCheckThrowOnFailure();
        }

        [TestMethod, ExpectedException(typeof(Exception))]
        public void Label()
        {
            Prop.ForAll<int>(x => false).Label("Always false")
               .And(x => Math.Abs(x) - x == 0)
               .QuickCheckThrowOnFailure();
        }
    }
}
