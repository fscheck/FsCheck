using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace FsCheck.NUnit.CSharpExamples.ClassesToTest
{
    public class Counter
    {
        private int n = 0;

        public void Inc()
        {
            n++;
        }

        public void Dec()
        {
            if (n > 2)
                n -= 2;
            else
                n--;
        }

        public int Get()
        {
            return n;
        }
    }
}
