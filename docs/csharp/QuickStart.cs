using FsCheck;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;
using global::Xunit;

namespace CSharp
{
    class QuickStart
    {
        public static void Samples([CallerFilePath] string file = "")
        {
            //[revRevIsOrig]
            Func<int[],bool> revRevIsOrig = xs => xs.Reverse().Reverse().SequenceEqual( xs );
            Check.Quick(revRevIsOrig);
            //[/revRevIsOrig]

            //[revIsOrig]
            Check.Quick<Func<int[],bool>>(xs => xs.Reverse().SequenceEqual(xs));
            //[/revIsOrig]

            //[revRevIsOrigFloat]
            Check.Quick<Func<double[], bool>>(xs => xs.Reverse().Reverse().SequenceEqual(xs));
            //[/revRevIsOrigFloat]
        }

        //[revRevIsOrigFact]
        [Fact]
        public void RevRevIsOrig(){
            Check.QuickThrowOnFailure<Func<int[], bool>>(xs => xs.Reverse().Reverse().SequenceEqual(xs));
        }
        //[/revRevIsOrigFact]
    }
}
