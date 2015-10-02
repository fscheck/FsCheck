using System;
using System.Linq;
using FsCheck.Xunit;

namespace FsCheck.XUnit.CSharpExamples
{
    public class ReverseFixture
    {
        [Property(QuietOnSuccess = true)]
        public bool Bcl(int[] xs)
        {
          return xs.Reverse().Reverse().SequenceEqual(xs);
        }

        [Property(QuietOnSuccess = true)]
        public void Bcl2(int[] xs)
        {
          if(true == xs.Reverse().Reverse().SequenceEqual(xs))
          {
            // all ok
          }
          else
          {
            throw new Exception("Fail at life");
          }
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

        [Property]
        public bool ShouldFail_Exception(int[] xs)
        {
            throw new InvalidOperationException("Test failed!");
        }
    }
}
