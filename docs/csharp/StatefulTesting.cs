using FsCheck;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharp.DocSnippets {

    public class Counter {
        private int n;

        public void Inc() {
            n++;
        }

        public void Dec() {
            if (n > 2)
                n -= 2;
            else
                n--;
        }

        public int Get() {
            return n;
        }
    }

    //[counterspec]
    public class CounterSpec : ICommandGenerator<Counter, int> {

        public Gen<Command<Counter, int>> Next(int value) {
            return Gen.Elements(new Command<Counter, int>[] { new Inc(), new Dec() });
        }

        public Counter InitialActual { get { return new Counter(); } }

        public int InitialModel { get { return 0; } }

        private class Inc : Command<Counter,int> {
            public override Counter RunActual(Counter c) {
                c.Inc();
                return c;
            }

            public override int RunModel(int m) {
                return m + 1;
            }

            public override Property Post(Counter c, int m) {
                return (m == c.Get()).ToProperty();
            }

            public override string ToString() {
                return "inc";
            }
        }

        private class Dec : Command<Counter,int>{
            public override Counter RunActual(Counter c) {
                c.Dec();
                return c;
            }

            public override int RunModel(int m) {
                return m - 1;
            }

            public override Property Post(Counter c, int m) {
                return (m == c.Get()).ToProperty();
            }

            public override string ToString() {
                return "dec";
            }
        }
    }
    //[/counterspec]

    class StatefulTesting {

        public static void Samples() {

            //[check]
            new CounterSpec()
                .ToProperty()
                .QuickCheck();
            //[/check]

        }

    }
}
