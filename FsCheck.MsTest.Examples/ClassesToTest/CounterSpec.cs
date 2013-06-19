using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using FsCheck.Fluent;

namespace FsCheck.MsTest.Examples.ClassesToTest
{
    public class CounterSpec : Commands.ISpecification<Counter, int>
    {
        public Gen<Commands.ICommand<Counter, int>> GenCommand(int value)
        {
            return Any.ValueIn(new Commands.ICommand<Counter, int>[] { new Inc(), new Dec() });
        }

        public Tuple<Counter, int> Initial()
        {
            return new Tuple<Counter, int>(new Counter(), 0);
        }

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

        private abstract class BaseCommand : Commands.ICommand<Counter, int>
        {
            public override bool Post(Counter c, int m)
            {
                return m == c.Get();
            }

            public override string ToString()
            {
                return GetType().Name;
            }
        }
    }
}
