using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NUnit.Framework;
using FsCheck.Fluent;
using FsCheck.NUnit.CSharpExamples.ClassesToTest;

namespace FsCheck.NUnit.CSharpExamples
{
    [TestFixture]
    public class Examples
    {
        //Simple boolean property
        [FsCheck.NUnit.Property]
        public bool RevRev(int[] xs)
        {
            return xs.Reverse().Reverse().SequenceEqual(xs);
        }

        //Using Spec fluent interface
        //TODO: UnbrowsableObject return types look ugly. Rename to smth like Spec or ISpec?
        [FsCheck.NUnit.Property(Verbose = true)]
        public UnbrowsableObject RevId()
        {
            return Spec.ForAny<int[]>(xs => xs.Reverse().SequenceEqual(xs));
        }

        //TODO: do not call Commands.asProperty. Implement check for ISpecification directly
        [FsCheck.NUnit.Property]
        public Gen<Rose<Result>> Counter()
        {
            return Commands.asProperty(new CounterSpec());
        }        
    }
}
