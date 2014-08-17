(*** hide ***)
#I @"../../src/FsCheck/bin/Release"
#r @"FsCheck"
open FsCheck
open System

(**
# Test data: generators, shrinkers and Arbitrary instances

Test data is produced by test data generators. FsCheck defines default 
generators for some often used types, but you can use your own, and 
will need to define your own generators for any new types you introduce.

Generators have types of the form `Gen<'a>`; this is a generator for values 
of type a. To build your own generators, a computation expression 
called `gen` is provided by FsCheck, and all the functions in the `Gen` module are at your disposal.

Shrinkers have types of the for `'a -> seq<'a>`; given a value, a shrinker 
produces a sequence of values that are in some way smaller than the given value. 
If FsCheck finds a set of values that falsify a given property, it will try 
to make that value smaller than the original (random) value by getting the 
shrinks for the value and trying each one in turn to check that the property 
is still false. If it is, the smaller value becomes the new counter example 
and the shrinking process continues with that value.

Shrinkers have no special support from FsCheck - this is not needed, 
since you have all you need in `seq` computation expressions and the `Seq` module.

Finally, an `Arbitrary<'a>` instance packages these two types together to be used in properties. 
FsCheck also allows you to register Arbitrary instances in a `Type` to `Arbitrary` dictionary. 
This dictionary is used to find an arbitrary instance for properties that have arguments, 
based on the argument's type.

Arbitrary instances have some helper functions in the `Arb` module.
    
## Generators

Generators are built from the function `choose`,  which makes a random choice of 
a value from an interval, with a uniform distribution. For example, to make 
a random choice between the elements of a list, use*)

let chooseFromList xs = 
  gen { let! i = Gen.choose (0, List.length xs-1) 
        return (List.nth xs i) }
  
(**  
### Choosing between alternatives

A generator may take the form `Gen.oneof <sequence of generators>`
which chooses among the generators in the list with equal probability. For example,*)

Gen.oneof [ gen { return true }; gen { return false } ]

(**
generates a random boolean which is true with probability one half.

We can control the distribution of results using `frequency`
instead. `frequency` chooses a generator from the list randomly, but weighs the probability of 
choosing each alternative by the factor given. For example,*)

Gen.frequency [ (2, gen { return true }); (1, gen { return false })]

(**
generates true two thirds of the time.
    
### The size of test data

Test data generators have an implicit size parameter; FsCheck begins by 
generating small test cases, and gradually increases the size as testing 
progresses. Different test data generators interpret the size parameter 
in different ways: some ignore it, while the list generator, for example, 
interprets it as an upper bound on the length of generated lists. You are 
free to use it as you wish to control your own test data generators.

You can obtain the value of the size parameter using `sized`. `sized g` calls `g`, passing it 
the current size as a parameter. For example, to generate natural
 numbers in the range 0 to size, use*)

Gen.sized <| fun s -> Gen.choose (0,s)

(**
The purpose of size control is to ensure that test cases are large enough to reveal errors, 
while remaining small enough to test fast. Sometimes the default size control does not achieve 
this. For example, towards the end of a test run arbitrary lists may have up to 50 elements, 
so arbitrary lists of lists may have up to 2500, which is too large for efficient testing. In 
such cases it can be useful to modify the size parameter explicitly. You can do so using 
`resize`.

`resize n g` invokes generator `g` with size parameter `n`. The size parameter should never be 
negative. For example, to generate a random matrix it might be appropriate to take the square 
root of the original size:*)

let matrix gen = Gen.sized <| fun s -> Gen.resize (s|>float|>sqrt|>int) gen

(**
### Generating recursive data types

Generators for recursive data types are easy to express using `oneof` or `frequency` to choose 
between constructors, and F#'s standard computation expression syntax to form a generator for each case. 
There are also `map` functions for arity up to 6 to lift constructors and functions into the `Gen` type. 
For example, if the type of trees is defined by *)

type Tree = Leaf of int | Branch of Tree * Tree

(**
then a generator for trees might be defined by*)

let rec unsafeTree() = 
  Gen.oneof [ Gen.map Leaf Arb.generate<int> 
              Gen.map2 (fun x y -> Branch (x,y)) (unsafeTree()) (unsafeTree())]

(**
However, a recursive generator like this may fail to terminate with a 
StackOverflowException, or produce very large results. To avoid this, 
recursive generators should always use the size control mechanism. For example,*)

let tree =
    let rec tree' s = 
        match s with
        | 0 -> Gen.map Leaf Arb.generate<int>
        | n when n>0 -> 
            let subtree = tree' (n/2)
            Gen.oneof [ Gen.map Leaf Arb.generate<int> 
                        Gen.map2 (fun x y -> Branch (x,y)) subtree subtree]
        | _ -> invalidArg "s" "Only positive arguments are allowed"
    Gen.sized tree'

(**
Note that

- We guarantee termination by forcing the result to be a leaf when the size is zero. 
- We halve the size at each recursion, so that the size gives an upper bound on the number of nodes in the tree. We are free to interpret the size as we will. 
- The fact that we share the subtree generator between the two branches of a Branch does not mean that we generate the same tree in each case.

    
### Useful Generator Combinators

If `g` is a generator for type `t`, then 

- `two g` generates a pair of t's, 
- `three g` generates a triple of t's, 
- `four g` generates a quadruple of t's, 
- If xs is a list, then `elements xs` generates an arbitrary element of xs.
- `listOfLength n g` generates a list of exactly n t's. 
- `listOf g` generates a list of t's whose length is determined by the size parameter
- `nonEmptyListOf g` generates a non-empty list of t's whose length is determined by the size parameter.
- `constant v` generates the value v.
- `suchThat p g` generates t's that satisfy the predicate p. Make sure there is a high chance that the predicate is satisfied.
- `suchThatOption p g` generates Some t's that satisfy the predicate p, and None if none are found. (After 'trying hard')


All the generator combinators are functions on the Gen module.
    
## Default Generators and Shrinkers based on type

FsCheck defines default test data generators and shrinkers for some often used typesm for example
unit, bool, byte, int, float, char, string, DateTime, lists, array 1D and 2D, Set, Map, objects and 
functions from and to any of the above. Furthermore, by using reflection, FsCheck can derive 
default implementations of record types, discriminated unions, tuples and enums in terms 
of any primitive types that are defined (either in FsCheck or by you).

You do not need to define these explicity for every property: FsCheck can provide a property with 
appropriate generators and shrinkers for all of the property's arguments, if it knows them or 
can derive them. Usually you can let type inference do the job of finding out these types 
based on your properties. However if you want to coerce FsCheck to use a particular generator 
and shrinker, you can do so by providing the appropriate type annotations.

FsCheck packages a generator and shrinker for a particular type in an `Arbitrary` type. You can 
provide FsCheck with an Arbitrary instance for your own types, by defining static members of a class, 
each of which should return an instance of a subclass of the class `Arbitrary<'a>`:*)

type MyGenerators =
  static member Tree() =
      {new Arbitrary<Tree>() with
          override x.Generator = tree
          override x.Shrinker t = Seq.empty }

(**
Replace the `'a` by the particular type you are defining an Arbitary instance for. 
Only the `Generator` method needs to be defined; `Shrinker` by default returns the empty 
sequence (i.e. no shrinking will occur for this type).

Now, to register all Arbitrary instances in this class:*)

Arb.register<MyGenerators>()

(**
FsCheck now knows about `Tree` types, and can not only generate Tree values, but also e.g. lists, tuples and 
option values containing Trees:*)

(***define-output:RevRevTree***)
let RevRevTree (xs:list<Tree>) = 
  List.rev(List.rev xs) = xs
Check.Quick RevRevTree

(***include-output:RevRevTree***)

(**
To generate types with a generic type argument, e.g.*)

type Box<'a> = Whitebox of 'a | Blackbox of 'a

(**
you can use the same principle. So the class `MyGenerators` can be writtten as follows:*)

let boxGen<'a> : Gen<Box<'a>> = 
    gen { let! a = Arb.generate<'a>
          return! Gen.elements [ Whitebox a; Blackbox a] }

type MyGenerators =
    static member Tree() =
        {new Arbitrary<Tree>() with
            override x.Generator = tree
            override x.Shrinker t = Seq.empty }
    static member Box() = Arb.fromGen boxGen

(**
Notice that we use the function `generate<'a>` from the Arb module to get the generator 
for the type argument of `Box`. This allows you to define generators recursively. Similarly, there is 
a function `shrink<'a>`. Look at the FsCheck source for examples of default Arbitrary implementations 
to get a feeling of how to write such Arbitrary instances. The Arb module should help you with this task as well.

Now, the following property can be checked:*)

(***define-output:RevRevBox***)
let RevRevBox (xs:list<Box<int>>) = 
  List.rev(List.rev xs) = xs
Check.Quick RevRevBox

(***include-output:RevRevBox***)

(**
Note that the class needs not be tagged with attributes in any way. FsCheck determines the type of 
the generator by the return type of each static member.

Also note that in this case we actually didn't need to write a generator or shrinker: FsCheck can 
derive suitable generators using reflection for discriminated unions, record types and enums.
    
## Useful methods on the Arb module

- `Arb.from<'a>` returns the registered Arbitrary instance for the given type 'a
- `Arb.fromGen` makes a new Arbitrary instance from just a given generator - the shrinker return the empty sequence
- `Arb.fromGenShrink` make a new Arbitrary instance from a given generator and shrinker. This is equivalent to implementing Arbitrary yourself, but may be shorter.
- `Arb.generate<'a>` returns the generator of the registered Arbitrary instance for the given type 'a
- `Arb.shrink` return the immediate shrinks of the registered Arbitrary instance for the given value
- `Arb.convert` given conversion functions to ('a ->'b) and from ('b ->'a), converts an Arbitrary<'a> instance to an Arbitrary<'b>
- `Arb.filter` filters the generator and shrinker for a given Arbitrary instance to contain only those values that match with the given filter function
- `Arb.mapFilter` maps the generator and filter the shrinkers for a given Arbitrary instance. Mapping the generator is sometimes faster, e.g. for a PositiveInt it is faster to take the absolute value than to filter the negative values.
- `Arb.Default` is a type that contains all the default Arbitrary instances as they are shipped and registerd by FsCheck by default. This is useful when you override a default generator - typically this is because you want to filter certain values from it, and then you need to be able to refer to the default generator in your overriding generator.*)
