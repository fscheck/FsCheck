# Properties

Properties are expressed as F# function definitions or C# lambdas or methods.
Properties are universally quantified over their parameters, so:

```fsharp
let revRevIsOrig (xs:list<int>) = List.rev(List.rev xs) = xs
```

static bool RevRevIsOriginal(int[] ts) {
    return ts.Reverse().Reverse().SequenceEqual(ts);
}
means that the equality holds for all lists xs.

Properties must not have generic types - because there can be so many different
kinds of constraints on generic types, some of which may not even be visible from the
type signature, we currently think allowing FsCheck to generate a generic type is not worth the added complexity.
It's very simple to fix any types anyway simply by adding some type annotations.

FsCheck can check properties of various forms - these forms are called testable,
and are indicated in the API by a generic type called `'Testable`. A `'Testable` may
be a function of any number of parameters that returns `bool` or `unit`. In the latter case,
a test passes if it does not throw. The entry point to create properties is the `Prop` module.

Like all of FsCheck's API, there are C# counterparts for all of the F# methods described.

## Conditional Properties

Properties may take the form `<condition> ==> <property>`

For example:

```fsharp
let insertKeepsOrder (x:int) xs = ordered xs ==> ordered (insert x xs)
Check.Quick insertKeepsOrder
```

Prop.ForAll<int, int[]>((x, xs) => xs.Insert(x).IsOrdered().When(xs.IsOrdered()))
    .QuickCheck();
```
Arguments exhausted after 18 tests.
```

Such a property holds if the property after `==>` holds whenever the condition does.

Testing discards test cases which do not satisfy the condition. Test case generation
continues until 100 cases which do satisfy the condition have been found, or until
an overall limit on the number of test cases is reached (to avoid looping if the condition
never holds). In this case a message such as "Arguments exhausted after 97 tests."
indicates that 97 test cases satisfying the condition were found, and that the property held in those 97 cases.

Notice that in this case the generated values had to be restricted to `int`. This is because the generated
values need to be comparable, but this is not reflected in the types. Therefore, without the explicit
restriction, FsCheck could generate lists containing different types (subtypes of objects), and these are not mutually comparable.

## Lazy Properties

Since F# has eager evaluation by default, the above property does more work than necessary:
it evaluates the property at the right of the condition no matter what the
outcome of the condition on the left. While only a performance consideration in the above
example, this may limit the expressiveness of properties - consider:

```fsharp
let tooEager a = a <> 0 ==> (1/a = 1/a)
Check.Quick tooEager
```

```
Falsifiable, after 33 tests (0 shrinks) (17787731155581572454,7198351959674246555). 
Last step was invoked with size of 34 and seed of (4085030360274601441,4426581254760786343).
Replay directly at failing step with (4085030360274601441,4426581254760786343,34).
Original:
0
with exception:
System.DivideByZeroException: Attempted to divide by zero.
   at FSI_0019.tooEager(Int32 a)
   at System.Lazy`1.ViaFactory(LazyThreadSafetyMode mode)
   at System.Lazy`1.CreateValue()
   at FsCheck.Testable.Prop.safeForce[a](Lazy`1 body) in /_//src/FsCheck/Testable.fs:line 166
```

Non-strict evaluation is needed here to make sure the property is checked correctly:

```fsharp
let moreLazy a = a <> 0 ==> (lazy (1/a = 1/a))
Check.Quick moreLazy
```

Prop.ForAll<int>(a => new Func<bool>(() => 1 / a == 1 / a).When(a != 0))
    .QuickCheck();
```
Ok, passed 100 tests.
```

## Quantified Properties

Properties may take the form `forAll <arbitrary>  (fun <args> -> <property>)`.

For example:

```fsharp
let orderedList = ArbMap.defaults |> ArbMap.arbitrary<list<int>> |> Arb.mapFilter List.sort ordered
let insertWithArb x = Prop.forAll orderedList (fun xs -> ordered(insert x xs))
Check.Quick insertWithArb
```

var orderedList = ArbMap.Default.ArbFor<int[]>()
                     .MapFilter(xs => xs.OrderBy(i => i).ToArray(), xs => xs.IsOrdered());

Prop.ForAll<int>(x => Prop.ForAll(orderedList, xs => xs.Insert(x).IsOrdered()))
    .QuickCheck();
```
Ok, passed 100 tests.
```

The first argument of `forAll` is an `IArbitrary` instance. Such an instance
encapsulates a test data generator and a shrinker (more on that in [Test Data](TestData.html)).
By supplying a custom generator, instead of using the default generator
for that type, it is possible to control the distribution of test data. In
the example, by supplying a custom generator for ordered lists, rather than
filtering out test cases which are not ordered, we guarantee that 100 test
cases can be generated without reaching the overall limit on test cases.
Combinators for defining generators are described in [Test Data](TestData.html).

## Expecting exceptions

You may want to test that a function or method throws an exception under certain circumstances.
Use `throws<'e :> exn,'a> Lazy<'a>` to achieve this. For example:

```fsharp
let expectDivideByZero() = Prop.throws<DivideByZeroException,_> (lazy (raise <| DivideByZeroException()))
Check.Quick expectDivideByZero
```

```
Ok, passed 100 tests.
```

```fsharp
  
```

This functionality is not available in the C# API.

## Observing Test Case Distribution

It is important to be aware of the distribution of test cases: if test data is not well
distributed then conclusions drawn from the test results may be invalid. In particular,
the `==>` operator can skew the distribution of test data badly, since only test data which
satisfies the given condition is used.

FsCheck provides several ways to observe the distribution of test data. Code for
making observations is incorporated into the statement of properties, each time
the property is actually tested the observation is made, and the collected observations
are then summarized when testing is complete.

### Counting Trivial Cases

A property may take the form `trivial <condition> <property>`

For example:

```fsharp
let insertTrivial (x:int) xs = 
  ordered xs ==> (ordered (insert x xs))
  |> Prop.trivial (List.length xs = 0)
Check.Quick insertTrivial
```

Prop.ForAll<int, int[]>((x, xs) =>
            xs.Insert(x).IsOrdered()
            .When(xs.IsOrdered())
            .Classify(xs.Count() == 0, "trivial"))
    .QuickCheck();
Test cases for which the condition is true are classified as trivial, and the proportion of
trivial test cases in the total is reported:

```
Arguments exhausted after 15 tests (60% trivial).
```

### Classifying Test Cases

A property may take the form `classify <condition> <string> <property>`

For example:

```fsharp
let insertClassify (x:int) xs = 
  ordered xs ==> (ordered (insert x xs))
  |> Prop.classify (ordered (x::xs)) "at-head"
  |> Prop.classify (ordered (xs @ [x])) "at-tail"
Check.Quick insertClassify
```

Prop.ForAll<int, int[]>((x, xs) =>
        xs.Insert(x).IsOrdered()
        .When(xs.IsOrdered())
        .Classify(new[] { x }.Concat(xs).IsOrdered(), "at-head")
        .Classify(xs.Concat(new int[] { x }).IsOrdered(), "at-tail"))
    .QuickCheck();
Test cases satisfying the condition are assigned the classification given, and the distribution of
classifications is reported after testing:

```
Arguments exhausted after 18 tests.
38% at-head.
33% at-tail, at-head.
27% at-tail.
```

Note that a test case may fall into more than one classification.

### Collecting Data Values

A property may take the form `collect <expression> <property>`

For example:

```fsharp
let insertCollect (x:int) xs = 
  ordered xs ==> (ordered (insert x xs))
      |> Prop.collect (List.length xs)
Check.Quick insertCollect
```

Prop.ForAll<int, int[]>((x, xs) =>
        xs.Insert(x).IsOrdered()
        .When(xs.IsOrdered())
        .Collect("length " + xs.Count().ToString()))
    .QuickCheck();
The argument of collect is evaluated in each test case, and the distribution of
values is reported. The type of this argument is printed using `sprintf "%A"`:

```
Arguments exhausted after 22 tests.
36% 2.
31% 1.
22% 0.
4% 4.
4% 3.
```

### Combining Observations

The observations described here may be combined in any way. All the observations
of each test case are combined, and the distribution of these combinations is
reported. For example:

```fsharp
let insertCombined (x:int) xs = 
    ordered xs ==> (ordered (insert x xs))
    |> Prop.classify (ordered (x::xs)) "at-head"
    |> Prop.classify (ordered (xs @ [x])) "at-tail"
    |> Prop.collect (List.length xs)
Check.Quick insertCombined
```

Prop.ForAll<int, int[]>((x, xs) =>
        xs.Insert(x).IsOrdered()
        .When(xs.IsOrdered())
        .Classify(new [] { x }.Concat(xs).IsOrdered(), "at-head")
        .Classify(xs.Concat(new int[] { x }).IsOrdered(), "at-tail")
        .Collect("length " + xs.Count().ToString()))
    .QuickCheck();
```
Arguments exhausted after 14 tests.
42% 0, at-tail, at-head.
21% 1, at-tail.
21% 1, at-head.
7% 4.
7% 2, at-tail.
```

## And, Or and Labels

Properties may take the form

* `<property> .&. <property>` succeeds if both succeed, fails if one of the properties fails, and is rejected when both are rejected.
* `<property> .|. <property>`succeeds if either property succeeds, fails if both properties fail, and is rejected when both are rejected.

The `.&.` combinator is most commonly used to write complex properties which share a generator.
In that case, it might be difficult upon failure to know excactly which sub-property has caused the failure.
That's why you can label sub-properties, and FsCheck shows the labels of the failed subproperties when
it finds a counter-example using `Prop.label`.

For example:

```fsharp
let complex (m: int) (n: int) =
  let res = n + m
  (res >= m)    |> Prop.label "result > #1" .&.
  (res >= n)    |> Prop.label "result > #2" .&. 
  (res < m + n) |> Prop.label "result not sum"
Check.Quick complex
```

Prop.ForAll<int, int>((m, n) => {
    var result = m + n;
    return (result >= m).Label("result > #1")
       .And(result >= n).Label("result > #2")
       .And(result < m + n).Label("result not sum");
}).QuickCheck();
```
Falsifiable, after 1 test (2 shrinks) (9646840292865275651,17898652977101466167). 
Last step was invoked with size of 2 and seed of (4920411409603297338,603253167685025285):
Label of failing property: result not sum
Original:
1
2
Shrunk:
0
0
with exception:
System.Exception: Expected true, got false.
```

It's perfectly fine to apply more than one label to a property; FsCheck displays all the applicable labels.
This is useful for displaying intermediate results, for example:

```fsharp
let multiply (n: int, m: int) =
    let res = n*m
    Prop.label (sprintf "evidence = %i" res) (
      Prop.label "div1" (m <> 0 ==> lazy (res / m = n)) .&. 
      Prop.label "div2" (n <> 0 ==> lazy (res / n = m)) .&. 
      Prop.label "lt1"  (res > m) .&. 
      Prop.label "lt2"  (res > n))
Check.Quick multiply
```

Prop.ForAll<int, int>((n, m) => {
    var res = n * m;
    return (new Func<bool>(() => res / m == n)).When(m != 0.0).Label("div1")
      .And((new Func<bool>(() => res / n == m)).When(n != 0.0).Label("div2"))
      .And((res > m).Label("lt1"))
      .And((res > n).Label("lt2"))
      .Label(string.Format("evidence = {0}", res));
}).QuickCheck();
```
Falsifiable, after 1 test (0 shrinks) (17535410725076243641,7858615317041592081). 
Last step was invoked with size of 2 and seed of (96525035778155449,9278704186832365995):
Labels of failing property (one or more is failing):
evidence = 0
lt1
lt2
Original:
(0, 0)
with exception:
System.Exception: Expected true, got false.
```
