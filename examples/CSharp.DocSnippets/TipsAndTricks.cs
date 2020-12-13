﻿using FsCheck;
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

            //[replay]
            Prop.ForAll((int x) => Math.Abs(x) >= 0)
                .Check(new Configuration { Replay = FsCheck.Random.StdGen.NewStdGen(1145655947, 296144285)});
            //[/replay]

            //[configuration]
            var configuration = Configuration.Quick;
            configuration.MaxNbOfTest = 1000;
            configuration.QuietOnSuccess = true;
            true.ToProperty().Check(configuration);
            //[/configuration]
        }
    }
}
