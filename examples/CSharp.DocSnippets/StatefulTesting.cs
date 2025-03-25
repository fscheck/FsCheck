namespace CSharp.DocSnippets;

using System;
using System.Collections.Generic;
using FsCheck;
using FsCheck.Experimental;
using FsCheck.Fluent;
using Microsoft.FSharp.Collections;
using Xunit;

public class StatefulTesting
{
    //[Counter]
    public class Counter
    {
        internal int N { get; private set; }

        internal Counter(int n)
        {
            N = n;
        }

        internal void Inc()
        {
            if (N <= 3)
                N += 1;
            else
                N = -N + 2;
        }

        internal void Dec()
        {
            if (N <= 0)
                throw new InvalidOperationException("Precondition fail");

            N -= 1;
        }

        internal void Reset()
        {
            N = 0;
        }

        public override string ToString() => $"Counter = {N}";
    }
    //[/Counter]

    //[Specification]
    private class CounterSpec
    {
        internal class Inc : Operation<Counter, int>
        {
            public override bool Pre(int m)
            {
                return m > 0;
            }

            public override int Run(int m)
            {
                return m + 1;
            }

            public override Property Check(Counter c, int m)
            {
                return (m == c.N).Label($"Inc: model = {m}, actual = {c.N}");
            }

            public override string ToString() => "inc";
        }

        internal class Dec : Operation<Counter, int>
        {
            public override bool Pre(int m)
            {
                return m > 0;
            }

            public override int Run(int m)
            {
                return m - 1;
            }


            public override Property Check(Counter c, int m)
            {
                c.Dec();
                return (m == c.N).Label($"Dec: model = {m}, actual = {c.N}");
            }

            public override string ToString() => "dec";
        }

        internal class CounterSetup : Setup<Counter, int>
        {
            private readonly int _initialValue;

            internal CounterSetup(int initialValue)
            {
                _initialValue = initialValue;
            }

            public override Counter Actual() => new(_initialValue);

            public override int Model() => _initialValue;
        }
    }

    public class Specification : Machine<Counter, int>
    {
        public Specification(int maxNumberOfCommands) : base(maxNumberOfCommands) { }

        public override Arbitrary<Setup<Counter, int>> Setup =>
            Arb.From(Gen.Choose(0, 3)
                .Select<int, Setup<Counter, int>>(i => new CounterSpec.CounterSetup(i)));

        public override TearDown<Counter> TearDown => new DisposeCall<Counter>();
        
        public override Gen<Operation<Counter, int>> Next(int _) =>
            Gen.Elements(new Operation<Counter, int>[]
            {
                new CounterSpec.Inc(), 
                new CounterSpec.Dec()
            });

        public override IEnumerable<FSharpList<Operation<Counter, int>>> ShrinkOperations(FSharpList<Operation<Counter, int>> operation) => 
            base.ShrinkOperations(operation);
    }
    //[/Specification]

    
    [Fact]
    void counter_tested_with_stateful_testing()
    {
        var property = new Specification(10).ToProperty();

        Check.QuickThrowOnFailure(property);
    }
}
