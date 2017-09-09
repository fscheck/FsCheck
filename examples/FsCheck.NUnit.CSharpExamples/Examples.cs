using System;
using System.Linq;
using FsCheck.NUnit.CSharpExamples.ClassesToTest;
using NUnit.Framework;

namespace FsCheck.NUnit.CSharpExamples
{
    [TestFixture]
    public class Examples
    {
        //Simple boolean property
        [Property]
        public bool RevRev(int[] xs)
        {
            return xs.Reverse().Reverse().SequenceEqual(xs);
        }

        // Note: should fail
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