using System;
using System.Linq;
using System.Threading.Tasks;
using FsCheck.Fluent;
using NUnit.Framework;

namespace FsCheck.NUnit.CSharpExamples
{
    [TestFixture]
    public class Examples
    {
        /// <summary> Simple boolean Function can be tested as a Delegate </summary>
        [Property]
        //[TestCase(new[] {1,2,3,4,5,6,7}, ExpectedResult = true)] unfortunately does NOT run both!
        public bool RevRev(int[] xs)
        {
            return xs.Reverse().Reverse().SequenceEqual(xs);
        }

        /// <summary> Example for using Property.ForAll; Note: should fail </summary>
        [Property(Verbose = true)]
        public Property RevId_shouldFail()
        {
            return Prop.ForAll<int[]>(xs => xs.Reverse().SequenceEqual(xs));
        }

        [Property(Replay = "54321,67585", Verbose = true)]
        public bool Replay(int x)
        {
            return Int32.MaxValue >= x;
        }

        /// <summary> Example for using Assert.Pass; Note: should pass </summary>
        [Property]
        public async Task AssertPass_shouldPass()
        {
            await Task.Yield();

            Assert.Pass("this test is successful");
        }

        /// <summary> Example for using Assert.Ignore; Note: test should be marked as Skipped in results </summary>
        [Property]
        public void AssertIgnore_should_be_Skipped()
        {
            Assert.Ignore("ignore this test");
        }

        /// <summary> Example for using Assert.Inconclusive; Note: test should be marked as Not Run in results </summary>
        [Property]
        public void AssertInconclusive_should_be_NotRun()
        {
            Assert.Inconclusive("this test is inconclusive");
        }
    }
}