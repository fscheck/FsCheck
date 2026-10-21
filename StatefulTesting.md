# Model-based Testing (Experimental)

*Caveat: since this is in the FsCheck.Experimental namespace, the rules of semantic versioning do
not apply to this particular part of the API. In other words, minor version releases may break your code.*

Let's look at the following simple class, which has an artificial bug:

```fsharp
type Counter(?initial:int) =
    let mutable n = defaultArg initial 0
    member __.Inc() = 
        //silly bug
        if n <= 3  then n <- n + 1 else n <- n + 2
        n
    member __.Dec() = if n <= 0 then failwithf "Precondition fail" else n <- n - 1; n
    member __.Reset() = n <- 0
    override __.ToString() = sprintf "Counter = %i" n
```

In C#:

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
As a model to test this class we can use an `int` value which is an abstraction of the object's internal state. The
idea is that each operation on the class (in this case, `Inc`, `Dec` and `Reset`) affects both the model object and the actual object, and
after each such operation, the model and the actual instance should still be equivalent.

With this idea in mind, you can write a specification of the `Counter` class using an int model as follows:

```fsharp
let spec =
    let inc = 
        { new Operation<Counter,int>() with
            member __.Run m = m + 1
            member __.Check (c,m) = 
                let res = c.Inc() 
                m = res 
                |> Prop.label (sprintf "Inc: model = %i, actual = %i" m res)
            override __.ToString() = "inc"}
    let dec = 
        { new Operation<Counter,int>() with
            member __.Run m = m - 1
            override __.Pre m = 
                m > 0
            member __.Check (c,m) = 
                let res = c.Dec()
                m = res 
                |> Prop.label (sprintf "Dec: model = %i, actual = %i" m res)
            override __.ToString() = "dec"}
    let create initialValue = 
        { new Setup<Counter,int>() with
            member __.Actual() = new Counter(initialValue)
            member __.Model() = initialValue }
    { new Machine<Counter,int>() with
        member __.Setup = Gen.choose (0,3) |> Gen.map create |> Arb.fromGen
        member __.Next _ = Gen.elements [ inc; dec ] }
```

In C#:

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
Let's break this down a bit. A specification is put together as a subtype of the abstract class `Machine<'TypeUnderTest,'ModelType>`.
What you're actually defining is a state machine which can simultaneously apply operations, or state transitions, to the actual system
under test (in this case, a simple object) and a model of the system under test.

The methods you override on `Machine` are `Setup`, `Next`, and optionally `TearDown`.

`Machine.Setup` is a property that returns an `Arbitrary` instance that generates (and optionally shrinks) a `Setup<'TypeUnderTest, 'ModelType>` object. This in turn has two methods
to override: `Actual()` which should return a new, fresh instance of the system under test every time it is called, and `Model()` which should return the corresponding
instance of the model object each time it is called. In the example, there is only one subclass of `Setup<'Counter,int>` (in more complex cases, there might be more). Our
`Setup` instance takes as argument the initial counter value, and returns a fresh `Counter` as the SUT. The model is in this case simply the initial value.

`Machine.Next` is a method that takes a model and generates the next operations that are possible given the current model state. Each operation
is represented by an `Operation` subclass.

`Operation` has three methods to override: `Run`, `Check` and optionally `Pre`. `Pre` takes a model and returns true
if and only if this operation can execute on a model in that state - in other words it checks if the precondition for the operation is satisfied.
`Machine.Next` can also return a reduced set of operations based on the model, which is more efficient, but the preconditions are checked regardless for each
operation `Next` generates.

`Run` returns the model that is the result of applying this operation to the given model. `Check` then takes the new model (as returned from `Run`),
applies the operation to the actual system under test, and checks whether the result of the SUT matches the model. The return type of `Check` is Property so
you can use the usual FsCheck `Prop` methods to implement this method.

In the example above there are only two operations that we're checking: `Inc` and `Dec`. The `Run` methods respectively increase and decrease the model. `Check`
calls `Inc()` or `Dec()` on the `Counter` instance, and checks that after that the model counter is equal to the real `Counter`. `Dec` in addition has a (quite
silly, for demonstration purposes) precondition that the value should be strictly greater than 0 - if we run this machine our `Counter` throws if we call `Dec`
so when we run this specification we can check that FsCheck does the right thing here (testing the test library, very meta...)

We also override `ToString` in each `Operation` so that counter-examples can be printed.

A specification can be checked as follows:

```fsharp
Check.Quick (StateMachine.toProperty spec)
```

```
Falsifiable, after 2 tests (1 shrink) (9461079782378382912,17206931349286825237). 
Last step was invoked with size of 3 and seed of (14812470506804053975,12378439887136101087).
Replay directly at failing step with (14812470506804053975,12378439887136101087,3).
Label of failing property: Inc: model = 5, actual = 6
Original:
(3, Setup Counter)
inc -> 4
inc -> 5
dec -> 4

Shrunk:
(3, Setup Counter)
inc -> 4
inc -> 5

with exception:
System.Exception: Expected true, got false.
```

Notice that not only has FsCheck found our 'bug', it has also produced the minimal sequence of operations that leads to it.

But what has actually happened? Using the generators from the `Machine` methods, FsCheck tries to generate a random sequence of operations, for example:

```fsharp
{Setup = (0, Setup Counter);
 Operations =
  [(inc, 1); (dec, 0); (inc, 1); (inc, 2); (inc, 3); (inc, 4); (inc, 5);
   (dec, 4); (dec, 3)];
 TearDown = TearDown Counter;}

```

You can read this as a trace of the operations: the counter started off in state `0`, then using operation `inc` when to state `1`, then using operation `dec`
went to state `0` and so on.

This sequence is first generated using the model only, i.e. no operations are actually applied to any `Counter` objects. After generating a full trace, the operations
are applied to the system under test, using the `Operation.Check` methods of the various `Operation` objects.

If a failing test is found, FsCheck will attempt to remove operations from the trace, as long as the test still fails. So in the example above,
although the original sequence contains a few superfluous operations, FsCheck normally finds a shorter if not the shortest sequence that leads to the failure.

Final tip: make the model class immutable. This makes it easier to reason about the model and the operations on it, and also makes it easier to write the `Check` methods.
If the model is mutable, you MUST make sure that the result of `Run` is a new instance of the model that you don't modify later on. FsCheck captures these results
during the test run and during shrinking, and relies on them not changing.
