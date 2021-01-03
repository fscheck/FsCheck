using FsCheck;
using System;
using System.Linq;
using System.Runtime.CompilerServices;
using global::Xunit;

namespace CSharp
{
    public class QuickStart
    {
        internal static void Samples([CallerFilePath] string file = "")
        {
            //[revRevIsOrig]
            static bool revRevIsOrig(int[] xs) => xs.Reverse().Reverse().SequenceEqual(xs);
            Prop.ForAll((Func<int[], bool>)revRevIsOrig).QuickCheck();
            //[/revRevIsOrig]

            //[revIsOrig]
            Prop.ForAll<int[]>(xs => xs.Reverse().SequenceEqual(xs))
                .QuickCheck();
            //[/revIsOrig]

            //[revRevIsOrigFloat]
            Prop.ForAll<double[]>(xs => xs.Reverse().Reverse().SequenceEqual(xs))
                .QuickCheck();
            //[/revRevIsOrigFloat]
        }

        //[revRevIsOrigFact]
        [Fact]
        public void RevRevIsOrig(){
            Prop.ForAll<int[]>(xs => xs.Reverse().Reverse().SequenceEqual(xs))
                .QuickCheckThrowOnFailure();
        }
        //[/revRevIsOrigFact]
    }
}
