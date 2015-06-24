using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace FsCheck.MsTest.Examples.ClassesToTest
{
    public class CounterSpec : CommandGenerator<Counter, int>
    {
        private Command<Counter, int> Inc =
                Command.FromFunc<Counter,int>("Inc", c => c + 1, (counter, i) => { counter.Inc(); return counter.Get() == i; });

        private readonly Command<Counter, int> Dec =
                Command.FromFunc<Counter,int>("Dec", c => c - 1, (counter, i) => { counter.Dec(); return counter.Get() == i; });

        private readonly Create<Counter, int> NewCounter =
                Command.Create(() => new Counter(), () => 0);

        public override Gen<Command<Counter, int>> Next(int value)
        {
            return Gen.Elements(Inc, Dec);
        }

        public override Gen<Create<Counter, int>> Create {
            get { return Gen.Constant(NewCounter); }
        }
    }
}
