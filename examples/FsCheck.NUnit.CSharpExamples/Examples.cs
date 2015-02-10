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

        //Using Spec fluent interface
        // Note: should fail
        [Property(Verbose = true)]
        public Specification RevId_shouldFail()
        {
            return Spec.ForAny<int[]>(xs => xs.Reverse().SequenceEqual(xs));
        }

        //TODO: do not call Commands.asProperty. Implement check for ISpecification directly
        // Note this one should fail
        [Property]
        public Gen<Rose<Result>> Counter_shouldFail()
        {
            return Commands.asProperty(new CounterSpec());
        }
    }
}