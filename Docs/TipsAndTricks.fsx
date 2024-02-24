(*** hide ***)
#I @"../src/FsCheck/bin/Release/netstandard2.0"
#r @"FsCheck"
open FsCheck
open FsCheck.FSharp
open System

(**
# Tips and Tricks
    
## Properties of functions

Perhaps surprisingly, FsCheck can generate random functions, `Func` and `Action`s. As a result, it can check properties of 
functions. For example, we can check associativity of function composition as follows:*)

(***define-output:associativity***)
let associativity (x:int) (f:int->float,g:float->char,h:char->int) = ((f >> g) >> h) x = (f >> (g >> h)) x
Check.Quick associativity

(***include-output:associativity***)

(**
FsCheck can generate all functions with a target type that it can generate. In addition, the functions are pure and total -
the former means that if you give a generated function the same value as input, it will keep returning that same value as output,
no matter how many times you call it. The latter means that the function does not throw any exceptions and always terminates.

If a counter-example is found, function values will be displayed as `<func>`. However, FsCheck can show 
you the generated function in more detail, if you ask it to generate a `Function` type, which has an embedded "real" function. 
FsCheck can even shrink `Function`s. For example:*)

(***define-output:mapRec***)
let mapRec (Fun f) (l:list<int>) =
  not l.IsEmpty ==>
      lazy (List.map f l = ((*f <|*) List.head l) :: List.map f (List.tail l))
Check.Quick mapRec

(***include-output:mapRec***)

(**
The type `Function<'a,'b>` - here deconstructed using the single case active pattern `Fun` - 
records a map of all the arguments it was called with, and the result it produced. 
In your properties, you can extract the actual function by pattern matching as in the example. 
`Function` is used to print the function, and also to shrink it.
    
## Use pattern matching instead of forAll to use custom generators

To define a generator that generates a subset of the normal range of values for an existing type,
say all the even ints, it makes properties more readable if you define a single-case union
case, and register a generator for the new type:
*)

(***define-output:EvenInt***)
type EvenInt = EvenInt of int with
  static member op_Explicit(EvenInt i) = i

type ArbitraryModifiers =
    static member EvenInt() = 
        ArbMap.defaults
        |> ArbMap.arbitrary<int> 
        |> Arb.filter (fun i -> i % 2 = 0) 
        |> Arb.convert EvenInt int
        
let ``generated even ints should be even`` (EvenInt i) = i % 2 = 0
Check.One(Config.Quick.WithArbitrary([typeof<ArbitraryModifiers>]), ``generated even ints should be even``)

(***include-output:EvenInt***)

(**
It's now easy to define custom shrink functions as well.

FsCheck uses this pattern frequently, e.g. `NonNegativeInt`, `PositiveInt`, `StringWithoutNullChars` etc. See the
default Arbitrary instances on the `Arb.Default` type.

Also, for these kinds of generators, the `Arb.filter`, `Arb.convert` and `Arb.mapFilter` functions will come in handy.
  
## An equality comparison that prints the left and right sides of the equality

Properties commonly check for equality. If a test case fails, FsCheck prints the counterexample, but 
sometimes it is useful to print the left and right side of the comparison, especially if you 
do some complicated calculations with the generated arguments first. To make this easier, you can 
define your own labelling equality combinator:*)

(***define-output:testCompare***)
let (.=.) left right = left = right |@ sprintf "%A = %A" left right

let testCompare (i:int) (j:int) = 2*i+1  .=. 2*j-1
Check.Quick testCompare

(***include-output:testCompare***)

(**
Of course, you can do this for any operator or function that you often use.
    
## Some ways to run FsCheck tests

* By adding properties and generators to an fsx file in your project. It's easy to execute, just press 
ctrl-a and alt-enter, and the results are displayed in F# Interactive. Be careful when referencing dlls 
that are built in your solution; Versions of F# Interactive earlier than 3.1.2 will lock those for the remainder of the session, 
and you won't be able to build until you quit the session. One solution is to include the source files 
instead of the dlls, but that makes the process slower. Useful for smaller projects. Difficult to debug though.
* By making a separate console application. Easy to debug, and no annoying locks on assemblies. Your best option 
if you use only FsCheck for testing and your properties span multiple assemblies.
* By using another unit testing framework. Useful if you have a mixed FsCheck/unit testing approach 
(some things are easier to check using unit tests, and vice versa), and you like a graphical runner. 
Depending on what unit testing framework you use, you may get good integration with Visual Studio for free. Also have a look
at some of the existing integrations with test runners like Xunit.NET, NUnit, Fuchu.

## Testing mutable types without using Command or StateMachine

For some relatively simple mutable types you might feel more comfortable just writing straightforward FsCheck properties without
using the `Command` or `StateMachine` API. This is certainly possible, but for shrinking FsCheck assumes that it can
re-execute the same test multiple times without the inputs changing. If you call methods or set properties on a generated object
that affect its state, this assumption does not hold and you'll see some weird results.

The simplest way to work around this is not to write a generator for your mutable object at all, but instead write an FsCheck property
that takes all the values necessary to construct the object, and then simply construct the object in the beginning of your test. For example, suppose we want to test
a mutable list:*)

let testMutableList =
    Prop.forAll (Arb.fromGen(Gen.choose (1,10))) (fun capacity -> 
        let underTest = new System.Collections.Generic.List<int>(capacity)
        Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary<int[]>) (fun itemsToAdd ->
            underTest.AddRange(itemsToAdd)
            underTest.Count = itemsToAdd.Length))

(**
    [lang=csharp,file=../examples/CSharp.DocSnippets/TipsAndTricks.cs,key=testMutableList]

This works, as a bonus you get shrinking for free.

If you do want to write a generator for your mutable type, this can be made to work but if
you mutate a generated object during a test, either:

* Disable shrinking, typically by wrapping all types into `DontShrink`; or
* Clone or otherwise 'reset' the generated mutable object at the beginning or end of every test.

## Replaying a failed test

When you have a failed test, it's often useful for debugging to be able to replay exactly those inputs. For this reason, FsCheck displays the
seed of its pseudo-random number generator when a test fails. Look for the bit of text that looks like: `(StdGen (1145655947,296144285))`.

To replay this test, which should have the exact same output, use the `Replay` field on `Config`:*)

Check.One(Config.Quick.WithReplay(1145655947UL,296144285UL), fun x -> abs x >= 0)

(**
In C#:

    [lang=csharp,file=../examples/CSharp.DocSnippets/TipsAndTricks.cs,key=replay]
*)

(**
## Checking properties in parallel

FsCheck can evaluate properties in parallel.
This feature may be useful to speed-up your cpu-heavy properties and custom arbitraries.
Also this is invaluable for running asynchronous propertiess, i.e. when you are doing asynchronous IO inside prop.
Don't forget to wrap your property in `Task` or `Async` in that case.

To run a property in parallel, use the `ParallelRunConfig` field on `Config`:*)

Check.One(
    Config.Quick.WithParallelRunConfig({ MaxDegreeOfParallelism = System.Environment.ProcessorCount }),
     fun x -> abs x >= 0
)

(**
`System.Environment.ProcessorCount` is a good default for cpu-bound work.
For io-bound work it's usually enough to set `ParallelRunConfig` to 1.
*)

Check.One(
    Config.Verbose.WithParallelRunConfig({ MaxDegreeOfParallelism = 1 } ),
    fun (x:int) -> 
        async { 
            do! Async.Sleep (abs x)
            return true
        }
)