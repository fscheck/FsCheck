using System;
using FsCheck;

namespace FsCheck.NUnit.CSharpExamples.ClassesToTest
{
    /// <summary>Oracle Test: Generates Sequences of <see cref="Inc"/> and <see cref="Dec"/> Commands to apply to the <see cref="Counter"/> and an <see cref="int"/>. </summary>
    public class CounterSpec : ICommandGenerator<Counter, int>
    {
        /// <summary> Factory for both Model and Oracle </summary>
        public Gen<Command<Counter, int>> Next(int value)
        {
            return Gen.Elements(new Command<Counter, int>[] {new Inc(), new Dec()});
        }

        /// <summary> SUT Start Value </summary>
        public Counter InitialActual { get { return new Counter();} }
    
        /// <summary> Model Start Value </summary>
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

        /// <summary> Common for all Commands </summary>
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