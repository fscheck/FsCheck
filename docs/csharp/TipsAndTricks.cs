using FsCheck;
using System;
using System.Collections.Generic;

namespace CSharp.DocSnippets
{
    class TipsAndTricks {

        public static void Samples() {

            //[testMutableList]
            Prop.ForAll(Arb.From(Gen.Choose(1, 10)), Arb.From<int[]>(),(capacity, itemsToAdd) => {
                var underTest = new List<int>(capacity);
                underTest.AddRange(itemsToAdd);
                return underTest.Count == itemsToAdd.Length;
            })
            .QuickCheck();
            //[/testMutableList]
        }
    }
}
