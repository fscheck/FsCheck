using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using FsCheck;

namespace FsCheck.CSharpExamples
{
    class Program
    {
        static void Main(string[] args)
        {
            var spec = new Spec();

            spec.For(Any.OfType<char>(), c => c.Equals('a'));
        }
    }
}
