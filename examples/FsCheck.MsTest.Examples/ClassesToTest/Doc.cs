using System;

namespace FsCheck.MsTest.Examples.ClassesToTest
{
    public abstract class Doc
    {
        public class Empty : Doc
        {            
        }

        public class Line : Doc
        {
        }

        public class Char : Doc
        {
            public char Value { get; private set; }

            public Char(char value)
            {
                Value = value;
            }

            public override string ToString()
            {
                return base.ToString() + "(" + Value + ")";
            }
        }

        public class Text : Doc
        {
            public string Value { get; private set; }

            public Text(string value)
            {
                Value = value;
            }

            public override string ToString()
            {
                return base.ToString() + "(" + Value + ")";
            }
        }

        public class Union : Doc
        {
            public Doc Doc1 { get; private set; }
            public Doc Doc2 { get; private set; }

            public Union(Doc doc1, Doc doc2)
            {
                Doc1 = doc1;
                Doc2 = doc2;
            }

            public override string ToString()
            {
                return base.ToString() + "(" + Doc1 + "," + Doc2 + ")";
            }
        }

        public class Concat : Doc
        {
            public Doc Doc1 { get; private set; }
            public Doc Doc2 { get; private set; }

            public Concat(Doc doc1, Doc doc2)
            {
                Doc1 = doc1;
                Doc2 = doc2;
            }

            public override string ToString()
            {
                return base.ToString() + "(" + Doc1 + "," + Doc2 + ")";
            }
        }

        public override string ToString()
        {            
            return GetType().Name;
        }
    }
}
