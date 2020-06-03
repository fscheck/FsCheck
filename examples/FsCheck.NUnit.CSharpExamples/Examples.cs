using System;
using System.Linq;
using FsCheck.NUnit.CSharpExamples.ClassesToTest;
using NUnit.Framework;

namespace FsCheck.NUnit.CSharpExamples
{
    [TestFixture]
    public class Examples
    {
        /// <summary> Simple boolean property can be tested as a Delegate </summary>
        /// <param name="xs"></param>
        /// <returns></returns>
        [Property]
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

        [Property(Replay = "54321,67584", Verbose = true)]
        public bool Replay(int x)
        {
            return Int32.MaxValue >= x;
        }

        //TODO: do not call toProperty.
        // Note this one should fail
        [Property]
        public Property Counter_shouldFail()
        {
            return new CounterSpec().ToProperty();
        }
    }
}