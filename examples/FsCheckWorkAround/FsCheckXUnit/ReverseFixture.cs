using System;
using System.Linq;
using FsCheck.Xunit;

namespace FsCheck.XUnit.CSharpExamples
{
    public class Positive
    {
        public static Arbitrary<int> Int32()
        {
          return Arb.Default.Int32().Filter(x => x > 0);
        }
    }

    public class Negative
    {
        public static Arbitrary<int> Int32()
        {
          return Arb.Default.Int32().Filter(x => x <= 0);
        }
    }

    // can run these tests via command line

    [Arbitrary(typeof(Negative))]
    public class ArbitraryTests
    {
        [Property(MaxTest=101)]
        public bool ShouldUseArbInstanceFromEnclosingClass(int i)
        {
          return i <= 0;
        }

        // This test is not showing up.
        [Property(Arbitrary = new Type[] { typeof(Positive) })]
        public bool ShouldUseArbInstanceOnMethod(int i)
        {
          return i > 0;
        }
    }

    public class ReverseFixture
    {
        [Property(Skip="Already under test")]
        public bool BclSkipped(int[] xs)
        {
          return xs.Reverse().Reverse().SequenceEqual(xs);
        }

        [Property]
        public bool Bcl(int[] xs)
        {
          return xs.Reverse().Reverse().SequenceEqual(xs);
        }

        [Property]
        public bool ShouldFail_1(int[] xs)
        {
          return xs.BadReverse1().SequenceEqual(xs.Reverse());
        }

        [Property]
        public bool ShouldFail_2(int[] xs)
        {
          return xs.BadReverse2().SequenceEqual(xs.Reverse());
        }

        [Property(MaxTest = 1000, EndSize = 5)]
        public bool ShouldFail_3(int[] xs)
        {
          return xs.BadReverse3().SequenceEqual(xs.Reverse());
        }
    }
}