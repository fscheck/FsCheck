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

            var gen = from x in Any.OfType<int>()
                      from y in Any.IntBetween(5, 10)
                      where x > 5
                      select new { Fst = x, Snd = y };


            Console.ReadKey();
        }
    }
}
