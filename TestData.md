# Test data: generators, shrinkers and Arbitrary instances

Test data is produced by test data generators. FsCheck defines default
generators for some often used types, but you can use your own, and
will need to define your own generators for any new types you introduce.

Generators have types of the form `Gen<'a>`; this is a generator for values
of type a. To build your own generators in F#, a computation expression
called `gen` is provided by FsCheck, and all the functions in the `Gen` module are at your disposal.
For C#, there are some LINQ methods you can use (select, where) and a number of methods on the `Gen` class.
The name for the methods in F# and C# are largely the same except for casing.

Shrinkers have types of the form `'a -> seq<'a>` aka `Func<T,IEnumerable<T>>`; given a value, a shrinker
produces a sequence of values that are (in some way) smaller than the given value.
If FsCheck finds a set of values that falsify a given property, it will try
to make those values smaller than the original (random) values by getting the
shrinks for the value and trying each one in turn to check that the property
is still false. If it is, the smaller value becomes the new counterexample
and the shrinking process continues with that value.

Shrinkers have no special support from FsCheck - this is not needed,
since you have all you need in `seq` computation expressions and the `Seq` module, or in LINQ and IEnumerable.

Finally, an `Arbitrary<'a>` instance packages a generator and shrinker together to be used in properties.
FsCheck also allows you to register Arbitrary instances in a `Type` to `Arbitrary` dictionary.
This dictionary is used to find an arbitrary instance for properties that have arguments,
based on the argument's type.

Arbitrary instances have some helper functions in `Arb`.

## Generators

Generators are built from the function `choose`,  which makes a random choice of
a value from an interval, with a uniform distribution. For example, to make
a random choice between the elements of a list, use

```fsharp
let chooseFromList xs = 
  gen { let! i = Gen.choose (0, List.length xs-1) 
        return List.item i xs }
```

public Gen<T> ChooseFrom<T>(T[] xs) {
    return from i in Gen.Choose(0,xs.Length-1)
           select xs[i];
}
### Choosing between alternatives

A generator may take the form `Gen.oneof <sequence of generators>`
which chooses among the generators in the list with equal probability. For example,
this generates a random boolean which is true with probability one half:

```fsharp
Gen.oneof [ gen { return true }; gen { return false } ]
```

var chooseBool = Gen.OneOf(Gen.Constant(true), Gen.Constant(false));
We can control the distribution of results using `frequency`
instead. `frequency` chooses a generator from the list randomly, but weighs the probability of
choosing each alternative by the factor given. For example, this generates true two thirds of the time.

```fsharp
Gen.frequency [ (2, gen { return true }); (1, gen { return false })]
```

var chooseBool2 = Gen.Frequency(
    (2, Gen.Constant(true)),
    (1, Gen.Constant(false)));
### The size of test data

Test data generators have an implicit size parameter; FsCheck begins by
generating small test cases, and gradually increases the size as testing
progresses. Different test data generators interpret the size parameter
in different ways: some ignore it, while the list generator, for example,
interprets it as an upper bound on the length of generated lists. You are
free to use it as you wish to control your own test data generators.

You can obtain the value of the size parameter using `sized`. `sized g` calls `g`, passing it
the current size as a parameter. For example, to generate natural
numbers in the range 0 to size, use

```fsharp
Gen.sized <| fun s -> Gen.choose (0,s)
```

var sizedInt = Gen.Sized(s => Gen.Choose(0, s));
The purpose of size control is to ensure that test cases are large enough to reveal errors,
while remaining small enough to test fast. Sometimes the default size control does not achieve
this. For example, towards the end of a test run arbitrary lists may have up to 50 elements,
so arbitrary lists of lists may have up to 2500, which is too large for efficient testing. In
such cases it can be useful to modify the size parameter explicitly. You can do so using
`resize`.

`resize n g` invokes generator `g` with size parameter `n`. The size parameter should never be
negative. For example, to generate a random matrix it might be appropriate to take the square
root of the original size:

```fsharp
let matrix gen = Gen.sized <| fun s -> Gen.resize (s|>float|>sqrt|>int) gen
```

public static Gen<T> Matrix<T>(Gen<T> generator) {
    return Gen.Sized(s => generator.Resize(Convert.ToInt32(Math.Sqrt(s))));
}
### Generating recursive data types

Generators for recursive data types are easy to express using `oneof` or `frequency` to choose
between constructors, and F#'s computation expressions or C# LINQ to form a generator for each case.
There are also `map` functions for arity up to 6 to lift constructors and functions into the `Gen` type.
For example, if the type of trees is defined by

```fsharp
type Tree = Leaf of int | Branch of Tree * Tree
```

then a generator for trees might be defined by

```fsharp
let rec unsafeTree() = 
  Gen.oneof [ ArbMap.defaults |> ArbMap.generate<int> |> Gen.map Leaf
              Gen.map2 (fun x y -> Branch (x,y)) (unsafeTree()) (unsafeTree())]
```

In C#, we elide the type because it is quite a bit more verbose than in F# - assume the typical composite
of having an abstract superclass Tree with two subclasses, one for Leaf and one for Branch. Basically this is
the code F# generates for the type definition above. Assuming that, `unsafeTree` in C# looks like:

public static Gen<Tree> UnsafeTree() {
    return Gen.OneOf(ArbMap.Default.GeneratorFor<int>().Select(i => (Tree) new Leaf(i)),
                     UnsafeTree().Two().Select(t => (Tree) new Branch(t.Item1,t.Item2)));
}
However, a recursive generator like this may fail to terminate with a
StackOverflowException, or produce very large results. To avoid this,
recursive generators should always use the size control mechanism:

```fsharp
let tree =
    let rec tree' s = 
        match s with
        | 0 -> ArbMap.defaults |> ArbMap.generate<int> |> Gen.map Leaf
        | n when n>0 -> 
            let subtree = tree' (n/2)
            Gen.oneof [ ArbMap.defaults |> ArbMap.generate<int> |> Gen.map Leaf
                        Gen.map2 (fun x y -> Branch (x,y)) subtree subtree]
        | _ -> invalidArg "s" "Only positive arguments are allowed"
    Gen.sized tree'
```

public static Gen<Tree> SafeTreeHelper(int size) {
    if (size == 0) {
        return ArbMap.Default.GeneratorFor<int>().Select(i => (Tree)new Leaf(i));
    }
    else {
        var subtree = SafeTreeHelper(size / 2);
        return Gen.OneOf(ArbMap.Default.GeneratorFor<int>().Select(i => (Tree) new Leaf(i)),
                         subtree.Two().Select(t => (Tree) new Branch(t.Item1,t.Item2)));
    }
}

public static Gen<Tree> SafeTree() {
    return Gen.Sized(SafeTreeHelper);
}
Note that

* We guarantee termination by forcing the result to be a leaf when the size is zero.
* We halve the size at each recursion, so that the size gives an upper bound on the number of nodes in the tree. We are free to interpret the size as we will.
* The fact that we share the subtree generator between the two branches of a Branch does not mean that we generate the same tree in each case.

### Useful Generator Combinators

If `g` is a generator for type `t`, then

* `two g` generates a pair of t's,
* `three g` generates a triple of t's,
* `four g` generates a quadruple of t's,
* If xs is a list, then `elements xs` generates an arbitrary element of xs.
* If xs is a list, then `growingElements xs` generates an arbitrary element among an initial segment of xs. The size of this initial segment increases with the size parameter.
* `listOfLength n g` generates a list of exactly n t's.
* `listOf g` generates a list of t's whose length is determined by the size parameter
* `nonEmptyListOf g` generates a non-empty list of t's whose length is determined by the size parameter.
* `constant v` generates the value v.
* `where p g` or `filter p g` generates t's that satisfy the predicate p. Make sure there is a high chance that the predicate is satisfied.
* `tryWhere p g` or `tryFilter p g` generates Some t's that satisfy the predicate p, and None if none are found. (After 'trying hard')
* If xs is a sequence, then `shuffle xs` generates a random permutation of xs.

All the generator combinators are functions on the Gen module. In C#, the names are the same just capitalized differently.

### Generator examples

The following examples use `Gen.sample` in order to show example output. In
general, you shouldn't use `Gen.sample` when writing properties, but it can be
helpful when developing or troubleshooting a useful custom generator.

Please be aware that due to the non-deterministic nature of FsCheck, the output
of calling `Gen.sample` will, in most cases, differ between calls.

The `Gen.sampleWithSize` function takes two arguments, in addition to the generator
from which it samples. The first argument is the [size](#The-size-of-test-data)
of the generated data. Some generators (like `Gen.constant` and `Gen.elements`)
don't use the `size` argument. For these generators, any integer value will do.

The second argument is the number of sample values to generate. Most examples
below use `Gen.sample` to generate a small list of example values, for example
a list of ten generated values.

#### Constant

The `Gen.constant` function is perhaps the simplest, and easiest, generator to
understand. Even though it's part of a system that generates random values,
this particular generator always returns the same value:

```fsharp
Gen.constant (1, "Foo") |> Gen.sampleWithSize 0 10
```

In this example, the constant is a complex value (a tuple); it can also be a
simple value, as for example a string or an integer. Since `Gen.constant`
doesn't rely on the `size` argument, it's `0` in this example, but any value
would do; it wouldn't change the result. As you can see from the return value,
all singular elements returned is the same tuple.

```
[|(1, "Foo"); (1, "Foo"); (1, "Foo"); (1, "Foo"); (1, "Foo"); (1, "Foo");
  (1, "Foo"); (1, "Foo"); (1, "Foo"); (1, "Foo")|]
```

Since the purpose of FsCheck is to generate random values, you shouldn't
need to use `Gen.constant` often. Still, it can come in handy if you need to
keep the value of a particular type constant while you vary other values.

#### Choose

You can use the `Gen.choose` function to create a generator of singular integer
values between a minimum and maximum value, both inclusive:

```fsharp
Gen.choose (0, 9) |> Gen.sampleWithSize 0 10
```

This example generates a single integer value between 0 and 9. Since
`Gen.choose` doesn't rely on the `size` argument, it's `0` in this example,
but any value would do; it wouldn't change the result.

While `Gen.choose (0, 9)` generates a single integer value, `Gen.sample 0 10`
generates 10 sample values:

```
[|4; 1; 8; 9; 5; 1; 7; 6; 0; 7|]
```

If you supply values in the 'wrong order', `Gen.choose` will follow
[Postel's law](https://en.wikipedia.org/wiki/Robustness_principle) and 'know
what you meant':

```fsharp
Gen.choose (99, 42) |> Gen.sampleWithSize 0 10
```

In this example, the first value is greater than the second value, but
`Gen.choose` will happily interpret this as a range, and produce values between
42 and 99, both included:

```
[|96; 69; 75; 74; 98; 81; 97; 97; 68; 99|]
```

Since both values are included, if you set both to the same value, you'll
effectively constrain the generator to that single value, and it'll behave like
`Gen.constant`.

#### Elements

You can use the `Gen.elements` function to create a generator of singular
values drawn from a collection of possible values. The collection is inclusive,
which means that both the first and last element, as well as all elements
between, can be drawn.

In the following example, a list of arbitrary integers define the collection of
possible values. The result is a generator that creates `int` values guaranteed
to be one of these values. Since `Gen.elements` doesn't rely on the `size`
argument, it's `0` in this example, but any value would do; it wouldn't change
the result.

```fsharp
Gen.elements [42; 1337; 7; -100; 1453; -273] |> Gen.sampleWithSize 0 10
```

The result of this expression is a list of ten sample values. Each value is
a single integer drawn from the collection of numbers:

```
[|1337; 1337; -100; 42; 1453; -100; 1453; 42; -273; 7|]
```

All elements are equally likely to be drawn from the collection; we say that
the random function has a uniform distribution. One easy way to affect the
distribution is to put more than one identical element into the collection:

```fsharp
Gen.elements ["foo"; "foo"; "bar"] |> Gen.sampleWithSize 0 10
```

In the above example, the value `"foo"` appears twice, so is twice as likely
to be drawn from the collection:

```
[|"foo"; "foo"; "foo"; "foo"; "foo"; "foo"; "foo"; "bar"; "foo"; "foo"|]
```

The above examples all use `list` values as input, but you can use any `seq`
expression, including `list` and `array` values, as long as the sequence is
finite.

#### GrowingElements

Essentially `Gen.growingElements` is like `Gen.elements` but also taking `size` into account.

You can use the `Gen.growingElements` function to create a generator of singular
values drawn *among an initial segment* of possible values. The size of this
initial segment increases with the `size` parameter.

In the following example, a list of ten characters define the collection of
possible values. The result is a generator that creates `char` values guaranteed
to be one of these values. Since `Gen.growingElements` relies on the `size`
argument, it's `3` in this example, which means only values from the segment
`['a'; 'b'; 'c']` will be returned.

```fsharp
Gen.growingElements ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'] |> Gen.sampleWithSize 3 10
```

The result of this expression is a list of ten sample values. Each value is
a single character drawn from the segment `['a'; 'b'; 'c']`:

```
[|'a'; 'c'; 'a'; 'a'; 'c'; 'b'; 'b'; 'c'; 'a'; 'b'|]
```

Let's run `Gen.growingElements` again, with the same input but with size `7`:

```fsharp
Gen.growingElements ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i'; 'j'] |> Gen.sampleWithSize 7 10
```

The result of this expression is a list of ten sample values. Each value is
now a single character drawn from the segment `['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g']`:

```
[|'b'; 'f'; 'a'; 'd'; 'c'; 'a'; 'a'; 'g'; 'g'; 'e'|]
```

The above examples all use `list` values as input, but you can use any `seq`
expression, including `list` and `array` values, as long as the sequence is
finite.

#### Map

Sometimes, you need to use a generator of one type in order to create a
generator of another type. For instance, you may need a `byte` value between 0
and 127. That sounds just like a job for `Gen.choose`, but unfortunately,
`Gen.choose (0, 127)` is a `Gen<int>`, and not a `Gen<byte>`. One way to
produce a `Gen<byte>` from a `Gen<int>` is to use `Gen.map`:

```fsharp
Gen.choose (0, 127) |> Gen.map byte |> Gen.sampleWithSize 0 10
```

This example uses the `byte` *function* to cast any `int` created by
`Gen.choose (0, 127)` to a `byte` value:

```
[|100uy; 7uy; 124uy; 91uy; 104uy; 16uy; 106uy; 108uy; 61uy; 76uy|]
```

This is only a basic example of the concept of `Gen.map`. In this particular
example, you could also have used `Gen.elements [0uy..127uy]` to achieve the
same result without using `Gen.map`, so let's consider a second
example.

Assume that you need to create a date in a particular month; e.g. November
2019. You can do that by creating an integer for the day of the month, and then
combine `Gen.map` with an anonymous function to get the desired date:

```fsharp
Gen.choose (1, 30)
|> Gen.map (fun i -> DateTime(2019, 11, i).ToString "u")
|> Gen.sampleWithSize 0 10
```

In this example, the generated `DateTime` value is immediately formatted as
a `string`, so that the output is more readable:

```
[|"2019-11-26 00:00:00Z"; "2019-11-04 00:00:00Z"; "2019-11-20 00:00:00Z";
  "2019-11-11 00:00:00Z"; "2019-11-26 00:00:00Z"; "2019-11-06 00:00:00Z";
  "2019-11-28 00:00:00Z"; "2019-11-19 00:00:00Z"; "2019-11-20 00:00:00Z";
  "2019-11-10 00:00:00Z"|]
```

This causes the resulting generator to have the type `Gen<string>`, but if
you omit calling `ToString "u"`, its type would have been `Gen<DateTime>`.

#### Lists

You can generate lists from individual value generators using `Gen.listOf`,
`Gen.listOfLength`, and `Gen.nonEmptyListOf`. These functions are
*combinators*, which means that they don't generate individual values
themselves, but rather use another generator to build values. For instance,
you can use `Gen.constant` to generate lists that all contain the same value:

```fsharp
Gen.constant 42 |> Gen.listOf |> Gen.sampleWithSize 1 10
```

This combination uses `Gen.constant 42` as an individual generator, and then
generates lists containing the number 42. While the value(s) in the list is
always 42, the length of the generated lists varies.

```
[|[42]; []; [42]; []; [42]; []; [42]; [42]; [42]; []|]
```

The length of the generated list is determined by the `size` argument. In
this example, the `size` argument is `1`, so the generated lists are short.
Note that while there's a correlation beteen `size` and the length of the
lists, you can't rely on a deterministic length. For that, there's
`Gen.listOfLength`:

```fsharp
Gen.choose (24, 42) |> Gen.listOfLength 5 |> Gen.sampleWithSize 0 10
```

This example uses `Gen.choose (24, 42)` in order to generate individual
integer values between 24 and 42. It then pipes this generator into
`Gen.listOfLength 5` in order to generate lists with exactly five elements:

```
[|[33; 34; 38; 29; 32]; [28; 37; 26; 37; 36]; [36; 32; 38; 28; 34];
  [29; 34; 27; 24; 27]; [34; 28; 26; 25; 36]; [34; 35; 42; 27; 31];
  [40; 37; 39; 26; 34]; [30; 40; 26; 28; 32]; [33; 35; 37; 29; 25];
  [40; 32; 24; 27; 42]|]
```

Notice that all sample lists have exactly five elements.

You can also use `Gen.nonEmptyListOf` to create lists that are guaranteed to
have at least one element. Like the other list generators, it uses a
single-value generator to generate its elements:

```fsharp
Gen.elements ["foo"; "bar"; "baz"] |> Gen.nonEmptyListOf |> Gen.sampleWithSize 20 4
```

Like `Gen.listOf`, `Gen.nonEmptyListOf` uses `size` to control the length
of the generated lists. They may still be small, but the larger the `size`
argument, the larger the lists may become.

```
[|["baz"; "bar"];
  ["baz"; "bar"; "baz"; "foo"; "bar"; "baz"; "bar"; "foo"; "baz"; "bar"; "baz";
   "baz"; "foo"; "baz"; "foo"; "baz"; "foo"]; ["bar"; "foo"; "baz"; "foo"];
  ["baz"; "bar"; "foo"; "bar"; "baz"; "bar"]|]
```

In this example, each element is drawn from the small set "foo", "bar", and
"baz". The lists are guaranteed to have at least a single element, but may be
longer.

#### Shuffle

You can use the `Gen.shuffle` function to create a generator that generates a
random permutation of a given finite sequence.

In the following example, the
[metasyntactic variables](https://en.wikipedia.org/wiki/Metasyntactic_variable)
"foo", "bar", "baz", and "qux" define the input sequence:

```fsharp
Gen.shuffle ["foo"; "bar"; "baz"; "qux"] |> Gen.sampleWithSize 0 6
```

Since `Gen.shuffle` doesn't rely on the `size` argument, it's `0` in this
example, but any value would do; it wouldn't change the result.

The result of this expression is a list of lists, where each list contains
the original input list, but shuffled:

```
[|[|"baz"; "foo"; "bar"; "qux"|]; [|"bar"; "qux"; "baz"; "foo"|];
  [|"bar"; "foo"; "qux"; "baz"|]; [|"foo"; "qux"; "baz"; "bar"|];
  [|"foo"; "qux"; "bar"; "baz"|]; [|"bar"; "baz"; "qux"; "foo"|]|]
```

The above example uses a `list` value as input, but you can use any `seq`
expression, including `list` and `array` values, as long as the sequence is
finite.

All shuffles are equally likely; the input order isn't excluded, so the
output may be the same as the input. Due to the nature of combinatorics, this
is more likely to happen the smaller the input list is.

#### Tuples

Sometimes you need to generate tuples of values. You can use the functions
`Gen.two`, `Gen.three`, and `Gen.four` to turn a single-value generator into a
generator of tuples.

Imagine that you need to generate two-dimensional points; you may, for
instance, be implementing
[Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life).
Points can be modelled as tuples of numbers. If you're modelling a grid, you
can use integers:

```fsharp
Gen.choose (-100, 100) |> Gen.two |> Gen.sampleWithSize 0 10
```

`Gen.two` uses a single-value generator to create a generator of two-element
tuples. This example generates 10 sample points, where each coordinate is
between -100 and 100:

```
[|(-87, 6); (80, 61); (-30, -62); (21, 81); (-15, 12); (100, -13); (-42, 5);
  (-2, 54); (85, -23); (-47, -78)|]
```

If you want to model a coordinate system in three-dimensional space, you may
decide to use floating points instead:

```fsharp
Gen.elements [-10.0..0.01..10.0] |> Gen.three |> Gen.sampleWithSize 0 10
```

In this example, you first use `Gen.elements` to draw a floating point value
from between -10 and 10, with two decimals; that defines a `Gen<float>`.
Second, `Gen.three` takes that `Gen<float>` and turns it into a
`Gen<float * float * float>`:

```
[|(-1.73, -3.21, -1.21); (3.68, 5.02, 5.99); (5.71, 0.17, 5.94);
  (-4.63, -7.54, -9.61); (8.26, -1.31, -7.52); (-4.24, -1.25, -6.46);
  (1.98, 0.47, -1.2); (-0.19, -3.34, -9.22); (-6.48, 5.4, 5.36);
  (-5.46, 6.56, 8.89)|]
```

Finally, `Gen.four` transforms a single-value generator into a generator of
four-element tuples. As all the other *combinators* in the `Gen` module, you
can combine it with other functions to define more specific values. Imagine,
for instance, that you need to create `System.Version` values. This type, which
captures a version of something, for example an operating system, or a library,
models version numbers as a composite of four numbers: *major*, *minor*,
*build*, and *revision* - all integers. One of the constructor overloads of
this class takes all four numbers, so you can combine `Gen.four` with `Gen.map`
to create Version values:

```fsharp
Gen.choose (0, 9)
|> Gen.four
|> Gen.map (System.Version >> string)
|> Gen.sampleWithSize 0 10
```

This example starts with `Gen.choose (0, 9)` to define a `Gen<int>` that
creates integer values betwen 0 and 9 (both included). Second, you pipe the
`Gen<int>` value into `Gen.four`, which returns a `Gen<int * int * int * int>`.
Third, you can pipe that generator into `Gen.map`, using the constructor
overload of `Version` that takes four integers; in F# 4, constructors can be
treated as functions, and a constructor with four arguments can be treated as a
function that takes a four-element tuple.

```
[|"4.2.5.3"; "3.2.6.7"; "9.9.6.0"; "6.8.7.0"; "2.9.0.2"; "2.2.0.9"; "2.5.0.6";
  "0.0.8.4"; "9.4.1.9"; "6.2.6.3"|]
```

This example composes the `Version` constructor with the `string` function,
in order to produce a more readable output. The resulting generator has the
type `Gen<string>`, but if you remove the `string` composition, the type would
be `Gen<Version>.`

#### Filter

While you can use the above generators and combinators to define various custom
rules for generating values, occasionally you have a requirement where the
easiest solution is to throw away some generated candidates. `Gen.filter` gives
you that opportunity.

Imagine, for example, that you have to create lists with two elements, but with
the restriction that the two elements must be different. One way to do that
could be to first generate a pair of values, and then use `Gen.filter` to
remove all pairs where the elements are equal. Subsequently, you can use
`Gen.map` to convert the pair to a list:

```fsharp
Gen.choose (1, 100)
|> Gen.two
|> Gen.filter (fun (x, y) -> x <> y)
|> Gen.map (fun (x, y) -> [x; y])
|> Gen.sampleWithSize 0 10
```

This expression generates 10 sample lists, each containing two different
numbers:

```
[|[4; 68]; [59; 66]; [8; 87]; [24; 27]; [79; 72]; [87; 30]; [46; 62]; [51; 29];
  [4; 99]; [63; 83]|]
```

When using `Gen.filter`, be sure to provide a predicate with a high chance
of returning `true`. If the predicate discards 'too many' candidates, it may
cause tests to run slower, or to not terminate at all. If your filter is
aggressive, consider using `Gen.tryFilter` instead of `Gen.filter`.

## Default Generators and Shrinkers based on type

FsCheck defines default test data generators and shrinkers for some often used types, for example
unit, bool, byte, int, float, char, string, DateTime, lists, array 1D and 2D, Set, Map, objects and
functions from and to any of the above. Furthermore, by using reflection, FsCheck can derive
default implementations of record types, discriminated unions, tuples, enums and basic classes in terms
of any primitive types that are defined (either in FsCheck or by you).

You do not need to define these explicity for every property: FsCheck can provide a property with
appropriate generators and shrinkers for all of the property's arguments, if it knows them or
can derive them. Usually you can let type inference do the job of finding out these types
based on your properties. However if you want to coerce FsCheck to use a particular generator
and shrinker, you can do so by providing the appropriate type annotations.

FsCheck packages a generator and shrinker for a particular type in an `Arbitrary` type. You can
provide FsCheck with an Arbitrary instance for your own types, by defining static members that
return an instance of a subclass of `Arbitrary<'a>`:

```fsharp
type MyGenerators =
  static member Tree() =
      {new Arbitrary<Tree>() with
          override _.Generator = tree
          override _.Shrinker _ = Seq.empty }
```

public class MyGenerators {
    public static Arbitrary<Tree> Trees() {
        return SafeTree().ToArbitrary();
    }
}
Replace the `'a` by the particular type you are defining an Arbitrary instance for.
Only the `Generator` method needs to be defined; `Shrinker` by default returns the empty
sequence which means no shrinking will be done for this type).

As the F# code shows, you can create your own subclass of `Arbitrary` and return it, or construct one from a generator
using `Arb.fromGen` or `Arb.fromGenShrink`. To retrieve an existing Arbitrary instance, use `ArbMap.arbitrary` with an
`IArbMap`, as shown below.

Now, to make FsCheck aware of all Arbitrary instances in this class, you can add it to the Config:

```fsharp
let myConfig = Config.Quick.WithArbitrary([ typeof<MyGenerators> ])
```

Check.One(Config.Default.WithArbitrary(new[] { typeof(MyGenerators) }), true);
If you use this config, FsCheck now knows about `Tree` types, and can not only generate Tree values, but also e.g. lists, tuples and
option values containing Trees:

```fsharp
let revRevTree (xs:list<Tree>) = 
  List.rev(List.rev xs) = xs
Check.One(myConfig, revRevTree)
```

```
Ok, passed 100 tests.
```

To generate types with a generic type argument, e.g.

```fsharp
type Box<'a> = Whitebox of 'a | Blackbox of 'a
```

you can use the same principle - except to write the generator and shrinker you now need an instance for whatever the generic parameter(s) will be
when building the generator. A factory method can take an argument of type `Arbitrary<'T>`; FsCheck supplies the instance
for the requested type from the configured `IArbMap`. For example, the class `MyGenerators` can be extended as follows:

```fsharp
let boxGen<'a> (contents:Arbitrary<'a>) : Gen<Box<'a>> = 
    gen { let! a = contents.Generator
          return! Gen.elements [ Whitebox a; Blackbox a] }

type MyTreeGenerator =
    static member Tree() =
        {new Arbitrary<Tree>() with
            override x.Generator = tree
            override x.Shrinker t = Seq.empty }
    static member Box(contents:Arbitrary<'a>) = contents |> boxGen |> Arb.fromGen
```

Now, when FsCheck examines the static methods on `MyTreeGenerator`, it injects the necessary `Arbitrary` instance. For example,
if we request to generate a `Box<int>`, FsCheck first finds the `Arbitrary` instance for `int`, and then calls `MyTreeGenerator.Box` with
that instance to get an `Arbitrary` instance for `Box<int>`. You can think of this as a parameter-driven dependency injection.

If the factory needs to look up more than one type, it can take the whole `IArbMap` instead and use `ArbMap.generate` or
`ArbMap.arbitrary` to retrieve the needed instances. For example, this factory uses the configured map to generate the
elements of a generic, non-empty list:

```fsharp
type NonEmptyList<'a> = NonEmptyList of 'a list

type NonEmptyListGenerators =
    static member NonEmptyList<'a>(map:IArbMap) : Arbitrary<NonEmptyList<'a>> =
        map
        |> ArbMap.generate<'a>
        |> Gen.nonEmptyListOf
        |> Gen.map NonEmptyList
        |> Arb.fromGen
```

The factory receives the map for the current test configuration, so element types can use either their default
Arbitrary instances or ones registered by the caller. `Arb.fromGen` does not provide shrinking; use `Arb.fromGenShrink`
if the custom type needs a shrinker.

Register the factory type with the configuration to use it for properties:

```fsharp
let nonEmptyListsAreNonEmpty (NonEmptyList (values:list<int>)) = not (List.isEmpty values)
Check.One(Config.Quick.WithArbitrary([typeof<NonEmptyListGenerators>]), nonEmptyListsAreNonEmpty)
```

```

```

This factory approach also supports recursively defined generic generators. Have a look at the FsCheck source for examples
of default Arbitrary implementations. The `Arb` module provides helpers for constructing and transforming instances.

Now, the following property can be checked:

```fsharp
let revRevBox (xs:list<Box<int>>) = 
  List.rev(List.rev xs) = xs
Check.One(Config.Quick.WithArbitrary([typeof<MyTreeGenerator>]), revRevBox)
```

```
Ok, passed 100 tests.
```

Note that the class needs not be tagged with attributes in any way. FsCheck determines the type of
the generator by the return type of each static member.

Also note that in this case we actually didn't need to write a generator or shrinker: FsCheck can
derive suitable instances using reflection for discriminated unions, record types and enums.

### Notes about the default Generators and Shrinkers

Most of the default Arbitrary instances are documented with xml comments that can be discovered via IntelliSense.
Here are some important things not mentioned in the xml comments to avoid duplication..

* Most of the default sized generators of the number-like types produce uniformly distributed values in size-dependent ranges specified in the comments.
* The DoNotSize generators of the number-like types except Decimal produce uniformly distributed values in the full range of the type.
* Most of the collection type generators are wrappers around F# list. You can assume that they are generated and shrunk the same way.
* The generators for reference types like string do not produce null by default. If that is desired, generate the `NullableRef<T>` type instead - it generates null 1/8 of the time.

## Useful functions in the Arb and ArbMap modules

* `Arb.fromGen gen` creates an `Arbitrary` instance from a generator. Its shrinker returns an empty sequence.
* `Arb.fromGenShrink (gen, shrinker)` creates an `Arbitrary` instance from a generator and shrinker.
* `Arb.convert to from arb` converts an `Arbitrary<'a>` instance to an `Arbitrary<'b>` using the supplied conversion functions.
* `Arb.filter predicate arb` filters the generator and shrinker of an `Arbitrary` instance to values that satisfy the predicate.
* `Arb.mapFilter mapper predicate arb` maps the generated values and filters them, and filters the shrinks using the predicate. Mapping can be more efficient than filtering alone; for example, a positive integer can be generated by taking the absolute value rather than rejecting negative values.
* `ArbMap.arbitrary<'a> arbMap` returns the `Arbitrary<'a>` instance from `arbMap`.
* `ArbMap.generate<'a> arbMap` returns the generator from the `Arbitrary<'a>` instance in `arbMap`.
* `ArbMap.defaults` is the immutable map of FsCheck's default Arbitrary instances. For example, `ArbMap.defaults |> ArbMap.arbitrary<int>` retrieves the default `Arbitrary<int>`. Use this when deriving a custom Arbitrary from a default one.
