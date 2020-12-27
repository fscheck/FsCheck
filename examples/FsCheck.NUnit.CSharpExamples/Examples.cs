using System;
using System.Linq;
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
    }
}