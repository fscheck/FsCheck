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
            Spec.For(Any.OfType<char>(), c => c.Equals('a'))
                 .When(c => c == 'a')
                 .AndFor(Any.OfType<int>(), i => i> 10)
                 .Classify((c,i) => i >5, "bigger")
                 .QuickCheck();

            Console.ReadKey();
        }
    }
}
