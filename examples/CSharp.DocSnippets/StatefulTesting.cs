using System;

namespace CSharp.DocSnippets;

public class StatefulTesting
{
    //[Counter]
    class Counter
    {
        private int _n;

        internal Counter(int n)
        {
            _n = n;
        }

        internal void Inc()
        {
            if (_n <= 3)
                _n += 1;
            else
                _n = -_n + 2;
        }

        internal void Dec()
        {
            if (_n <= 0)
                throw new InvalidOperationException("Precondition fail");

            _n -= 1;
        }

        internal void Reset()
        {
            _n = 0;
        }

        public override string ToString() => $"Counter = {_n}";
    }
    //[/Counter]
}
