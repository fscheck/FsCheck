using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace FsCheck.MsTest.Examples.ClassesToTest
{
    public class CounterSpec : ICommandGenerator<Counter, int>
    {
        private readonly Command<Counter, int> Inc =
                Command.Create<Counter, int>(c => { c.Inc(); return c; },
                                         c => c + 1,
                                         (counter, i) => counter.Get() == i);

        private readonly Command<Counter, int> Dec =
                Command.Create<Counter, int>(c => { c.Dec(); return c; },
                                         c => c - 1,
                                         (counter, i) => counter.Get() == i);

        public Gen<Command<Counter, int>> Next(int value)
        {
            return Gen.Elements(Inc, Dec);
        }

        public Gen<Counter> InitialActual { get { return Gen.Constant(new Counter());}}

        public int InitialModel { get { return 0; } }
    }
}
