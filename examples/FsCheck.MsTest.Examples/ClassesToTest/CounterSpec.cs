using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace FsCheck.MsTest.Examples.ClassesToTest
{
    public class CounterSpec : ICommandGenerator<Counter, int>
    {
        public Gen<Command<Counter, int>> Next(int value)
        {
            return Gen.Elements(new Command<Counter, int>[] { new Inc(), new Dec() });
        }

        public Counter InitialActual { get { return new Counter();}}

        public int InitialModel { get { return 0; } }

        private class Inc : BaseCommand
        {
            public override Counter RunActual(Counter c)
            {
                c.Inc();
                return c;
            }

            public override int RunModel(int m)
            {
                return m + 1;
            }            
        }

        private class Dec : BaseCommand
        {
            public override Counter RunActual(Counter c)
            {
                c.Dec();
                return c;
            }

            public override int RunModel(int m)
            {
                return m - 1;
            }
        }

        private abstract class BaseCommand : Command<Counter, int>
        {
            public override Gen<Rose<Result>> Post(Counter c, int m)
            {
                return Prop.ForAll(m == c.Get()).Build();
            }

            public override string ToString()
            {
                return GetType().Name;
            }
        }
    }
}
