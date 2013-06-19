using System;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using FsCheck.Fluent;
using FsCheck.MsTest.Examples.ClassesToTest;

namespace FsCheck.MsTest.Examples
{
    [TestClass]
    public class SampleTests
    {
        [TestMethod]
        public void RevRev()
        {
            Spec.ForAny<int[]>(xs => xs.Reverse().Reverse().SequenceEqual(xs)).
                Assert();
        }

        [TestMethod, ExpectedException(typeof(AssertFailedException))]
        public void RevId()
        {
            Spec.ForAny<int[]>(xs => xs.Reverse().SequenceEqual(xs))
                .AssertVerbose();
        }

        [TestMethod, ExpectedException(typeof(AssertFailedException))]
        public void Insert()
        {            
             Spec.ForAny<int, int[]>((x, xs) => xs.Insert(x).IsOrdered())
                .When((x, xs) => xs.IsOrdered())                
                .Assert();
        }

        [TestMethod]
        public void DivByZero()
        {
            Spec.ForAny<int>(a => 1 / a == 1 / a)
                .When(a => a != 0)
                .Assert();
        }

        [TestMethod, ExpectedException(typeof(AssertFailedException))]
        public void InsertTrivial()
        {
            Spec.ForAny<int, int[]>((x, xs) => xs.Insert(x).IsOrdered())
                .When((x, xs) => xs.IsOrdered())
                .Classify( (x,xs) => xs.Count() == 0, "trivial")
                .Assert();            
        }

        [TestMethod, ExpectedException(typeof(AssertFailedException))]
        public void InsertClassify()
        {
            Spec.ForAny<int, int[]>((x, xs) => xs.Insert(x).IsOrdered())
                .When((x, xs) => xs.IsOrdered())
                .Classify((x, xs) => new int[] { x }.Concat(xs).IsOrdered(), "at-head")
                .Classify((x, xs) => xs.Concat(new int[] { x }).IsOrdered(), "at-tail")
                .Assert();            
        }

        [TestMethod, ExpectedException(typeof(AssertFailedException))]
        public void InsertCollect()
        {
            Spec.ForAny<int, int[]>((x, xs) => xs.Insert(x).IsOrdered())
                .When((x, xs) => xs.IsOrdered())
                .Collect((x, xs) => "length " + xs.Count().ToString())
                .Assert();
        }

        [TestMethod, ExpectedException(typeof(AssertFailedException))]
        public void InsertCombined()
        {            
            Spec.ForAny<int, int[]>((x, xs) => xs.Insert(x).IsOrdered())
                .When((x, xs) => xs.IsOrdered())
                .Classify((x, xs) => new int[] { x }.Concat(xs).IsOrdered(), "at-head")
                .Classify((x, xs) => xs.Concat(new int[] { x }).IsOrdered(), "at-tail")
                .Collect((x, xs) => "length " + xs.Count().ToString())
                .Assert();
        }

        [TestMethod, ExpectedException(typeof(AssertFailedException))]
        public void ComplexProp()
        {
            Spec.ForAny<int, int>((m, n) => m + n >= m).Label("result > #1")
                .And((m, n) => m + n >= n, "result > #2")
                .And((m, n) => m + n < m + n, "result not sum")                
                .Assert();
        }

        [TestMethod, ExpectedException(typeof(AssertFailedException))]
        public void Label()
        {
            Spec.ForAny<int>(x => false).Label("Always false")
               .And(x => Math.Abs(x) - x == 0)
               .Assert();
        }
    }
}
