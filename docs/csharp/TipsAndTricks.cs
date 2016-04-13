using FsCheck;
using System.Collections.Generic;

namespace CSharp.DocSnippets
{
    class TipsAndTricks {
        //[testMutableList]
        public bool TestMutableList(PositiveInt capacity, int[] itemsToAdd) {
            var underTest = new List<int>(capacity.Get);
            underTest.AddRange(itemsToAdd);
            return underTest.Count == itemsToAdd.Length;
        }
        //[/testMutableList]
    }
}
