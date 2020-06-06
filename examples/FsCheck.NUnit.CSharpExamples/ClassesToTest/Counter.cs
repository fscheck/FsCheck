namespace FsCheck.NUnit.CSharpExamples.ClassesToTest
{
    /// <summary> Erroneous Counter Implementation </summary>
    public class Counter
    {
        private int n;

        /// <summary> Proper Increment by 1 </summary>
        public void Inc()
        {
            n++;
        }

        /// <summary> Wrong Decrement by 2 when larger than 2 </summary>
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