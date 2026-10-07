using System;
using System.Linq;
using System.Threading.Tasks;
using FsCheck.Xunit;

namespace FsCheck.XUnit.CSharpExamples
{
    public class ReverseFixture
    {
        [Property(QuietOnSuccess = true, EndSize = 10000)]
        public Task<bool> ShouldFail_Task(int i)
        {
            return Task.FromResult(i < 2000);
        }

        [Property(QuietOnSuccess = true, EndSize = 1000)]
        public async Task<bool> ShouldFail_TaskDelay(int i)
        {
            await Task.Delay(TimeSpan.FromSeconds(2)).ConfigureAwait(false);
            return false;
        }

        [Property(QuietOnSuccess = true)]
        public bool ShouldPass_Bool(int[] xs)
        {
            return xs.Reverse().Reverse().SequenceEqual(xs);
        }

        [Property(QuietOnSuccess = true)]
        public void ShouldPass_Void(int[] xs)
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
