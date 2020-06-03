namespace FsCheck.MsTest.Examples.ClassesToTest
{
    /// <summary> Generator which creates a random Sequence of <see cref="Inc"/> and <see cref="Dec"/> and applies them to <see cref="Counter"/> and <see cref="int"/> respectively. </summary>
    /// <remarks>
    /// Similar but different Strategy as in NUnit Sample where Commands derive from <see cref="Command{Actual,Model}"/>
    /// </remarks>
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

        public Counter InitialActual { get { return new Counter();}}

        public int InitialModel { get { return 0; } }
    }
}
