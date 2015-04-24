using System;
using FsCheck;

namespace FsCheck.NUnit.CSharpExamples.ClassesToTest
{
    public class CounterSpec : ICommandGenerator<Counter, int>
    {
        public Gen<Command<Counter, int>> Next(int value)
        {
            return Gen.Elements(new Command<Counter, int>[] {new Inc(), new Dec()});
        }

        public Counter InitialActual { get { return new Counter();} }
    
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
            public override Property Post(Counter c, int m)
            {
                return (m == c.Get()).ToProperty();
            }

            public override string ToString()
            {
                return GetType().Name;
            }
        }
    }
}