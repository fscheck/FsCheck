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
    public class CounterSpec : CommandGenerator<Counter, int> {

        public override Gen<Command<Counter, int>> Next(int value) {
            return Gen.Elements(new Command<Counter, int>[] { new Inc(), new Dec() });
        }

        private class NewCounter : Create<Counter, int> {

            public override Counter Actual() {
                return new Counter();
            }

            public override int Model() {
                return 0;
            }
        }

        private class Inc : Command<Counter,int> {

            public override int RunModel(int m) {
                return m + 1;
            }

            public override Property Check(Counter counter, int value) {
                counter.Inc();
                return (counter.Get() == value).ToProperty();
            }

            public override string ToString() {
                return "inc";
            }
        }

        private class Dec : Command<Counter,int>{

            public override int RunModel(int m) {
                return m - 1;
            }

            public override string ToString() {
                return "dec";
            }

            public override Property Check(Counter counter, int value) {
                counter.Dec();
                return (counter.Get() == value).ToProperty();
            }
        }

        public override Gen<Create<Counter, int>> Create {
            get { return Gen.Constant((Create<Counter,int>)new NewCounter()); }
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
