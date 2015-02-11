using System;
using System.Linq;
using System.Collections.Generic;
using FsCheck;
using FsCheck.MsTest.Examples.ClassesToTest;

namespace FsCheck.MsTest.Examples
{
    public static class DocGenenerator
    {
        public static Gen<Doc> Generator(int depth)
        {
            if (depth == 0)
                return Gen.OneOf(NonRecursiveDocGenerators());

            return Gen.OneOf(AllDocGenerators(depth / 2));
        }

        private static IEnumerable<Gen<Doc>> NonRecursiveDocGenerators()
        {
            return new Gen<Doc>[]
            {
                Constant<Doc.Empty>(),
                Constant<Doc.Line>(),
                from x in Arb.Generate<char>() select new Doc.Char(x) as Doc,
                from s in Arb.Generate<string>() select new Doc.Text(s) as Doc,
            };
        }

        private static Gen<Doc> Constant<T>()
            where T : Doc, new()
        {
            return Gen.Constant(new T() as Doc);
        }
       
        private static IEnumerable<Gen<Doc>> RecursiveDocGenerators(int depth)
        {
            return new Gen<Doc>[]
            {
                from doc1 in Generator(depth)
                from doc2 in Generator(depth)
                select new Doc.Union(doc1, doc2) as Doc,

                from doc1 in Generator(depth)
                from doc2 in Generator(depth)
                select new Doc.Concat(doc1, doc2) as Doc
            };
        }

        private static IEnumerable<Gen<Doc>> AllDocGenerators(int depth)
        {
            return Enumerable.Concat(NonRecursiveDocGenerators(), RecursiveDocGenerators(depth));            
        }
    }
}
