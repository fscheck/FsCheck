using System;
using System.Linq;
using FsCheck.Fluent;
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

        [TestMethod]
        public void RevId()
        {
            Assert.ThrowsExactly<Exception>(() =>
            {
            Prop.ForAll<int[]>(xs => xs.Reverse().SequenceEqual(xs))
                .VerboseCheckThrowOnFailure();
            });
        }

        [TestMethod]
        public void Insert()
        {
            Assert.ThrowsExactly<Exception>(() =>
            {            
             Prop.ForAll<int, int[]>((x, xs) =>
                        xs.Insert(x).IsOrdered()
                        .When(xs.IsOrdered()))
                .QuickCheckThrowOnFailure();
            });
        }

        [TestMethod]
        public void DivByZero()
        {
            Prop.ForAll<int>(a => new Func<bool>(() => 1 / a == 1 / a).When(a != 0))
                .QuickCheckThrowOnFailure();
        }

        [TestMethod]
        public void InsertTrivial()
        {
            Assert.ThrowsExactly<Exception>(() =>
            {
            Prop.ForAll<int, int[]>((x, xs) =>
                    xs.Insert(x).IsOrdered()
                    .When(xs.IsOrdered())
                    .Classify(xs.Count() == 0, "trivial"))
                .QuickCheckThrowOnFailure();
            });
        }

        [TestMethod]
        public void InsertClassify()
        {
            Assert.ThrowsExactly<Exception>(() =>
            {
            Prop.ForAll<int, int[]>((x, xs) =>
                    xs.Insert(x).IsOrdered()
                    .When(xs.IsOrdered())
                    .Classify(new[] { x }.Concat(xs).IsOrdered(), "at-head")
                    .Classify(xs.Concat(new int[] { x }).IsOrdered(), "at-tail"))
                .QuickCheckThrowOnFailure();
            });
        }

        [TestMethod]
        public void InsertCollect()
        {
            Assert.ThrowsExactly<Exception>(() =>
            {
            Prop.ForAll<int, int[]>((x, xs) =>
                    xs.Insert(x).IsOrdered()
                    .When(xs.IsOrdered())
                    .Collect("length " + xs.Count().ToString()))
                .QuickCheckThrowOnFailure();
            });
        }

        [TestMethod]
        public void InsertCombined()
        {
            Assert.ThrowsExactly<Exception>(() =>
            {
            Prop.ForAll<int, int[]>((x, xs) =>
                    xs.Insert(x).IsOrdered()
                    .When(xs.IsOrdered())
                    .Classify(new[] { x }.Concat(xs).IsOrdered(), "at-head")
                    .Classify(xs.Concat(new int[] { x }).IsOrdered(), "at-tail")
                    .Collect("length " + xs.Count().ToString()))
                .QuickCheckThrowOnFailure();
            });
        }

        [TestMethod]
        public void ComplexProp()
        {
            Assert.ThrowsExactly<Exception>(() =>
            {
            Prop.ForAll<int, int>((m, n) => {
                    var result = m + n;
                    return ( result >= m).Label("result > #1")
                       .And( result >= n).Label("result > #2")
                       .And( result < m + n).Label("result not sum");
            }).QuickCheckThrowOnFailure();
            });
        }

        [TestMethod]
        public void Label()
        {
            Assert.ThrowsExactly<Exception>(() =>
            {
            Prop.ForAll<int>(x => false.Label("Always false")
                            .And(Math.Abs(x) - x == 0))
               .QuickCheckThrowOnFailure();
            });
        }
    }
}
