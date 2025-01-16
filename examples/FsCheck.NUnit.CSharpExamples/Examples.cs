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

        // Fails as expected.
        [Property]
        public bool Test1(bool b) =>
            b && !b;

        // Fails as expected.
        [Property]
        public Property Test2(bool b) =>
            Prop.Label(b && !b, "Some label.");

        // Fails as expected.
        [Property]
        public Task<bool> Test3(bool b) =>
            Task.FromResult(b && !b);

        // Passes. I would expect this to fail.
        [Property]
        public Task<Property> Test4(bool b) =>
            Task.FromResult(Prop.Label(b && !b, "Some label."));

        // Passes. I would expect this to fail.
        [Property]
        public Task<Property> Test5(bool b) =>
            Task.FromResult(Prop.ToProperty(b).And(!b));

    }
}