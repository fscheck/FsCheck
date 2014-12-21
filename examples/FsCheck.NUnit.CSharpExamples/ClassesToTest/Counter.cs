namespace FsCheck.NUnit.CSharpExamples.ClassesToTest
{
    public class Counter
    {
        private int n;

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