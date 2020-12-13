﻿using FsCheck;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;
using global::Xunit;

namespace CSharp
{
    public class QuickStart
    {
        internal static void Samples([CallerFilePath] string file = "")
        {
            //[revRevIsOrig]
            Func<int[],bool> revRevIsOrig = xs => xs.Reverse().Reverse().SequenceEqual( xs );
            Prop.ForAll(revRevIsOrig).QuickCheck();
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
